#include <algorithm>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
#include <utility>
#include <cstdlib>

struct Port {
    std::string direction;
    std::string datatype;
    std::string name;
};

struct ModuleContext {
    std::string moduleName;
    std::filesystem::path dutSource;
    std::vector<Port> ports;
    std::optional<std::string> clockSignal;
    std::optional<std::string> resetSignal;
    bool resetActiveLow{false};
};

namespace {
std::string escapeRegex(const std::string &text) {
    std::string escaped;
    escaped.reserve(text.size() * 2);
    for (char ch : text) {
        if (std::isalnum(static_cast<unsigned char>(ch)) || ch == '_') {
            escaped.push_back(ch);
        } else {
            escaped.push_back('\\');
            escaped.push_back(ch);
        }
    }
    return escaped;
}

/*
 * escapeRegex
 * ----------------
 * Create a regex-escaped version of `text` suitable for inserting into
 * std::regex patterns. Non-alphanumeric and non-underscore characters
 * are backslash-escaped. This keeps regex searches for literal module
 * names safe when the name contains special characters.
 *
 * Params:
 *   text - the input string to escape
 * Returns:
 *   Escaped string where special characters are prefixed with '\\'.
 */


std::string trim(const std::string &text) {
    const auto first = text.find_first_not_of(" \t\n\r");
    if (first == std::string::npos) {
        return "";
    }
    const auto last = text.find_last_not_of(" \t\n\r");
    return text.substr(first, last - first + 1);
}

/*
 * trim
 * ----------------
 * Remove leading and trailing whitespace from a string.
 *
 * Params:
 *   text - input string to trim
 * Returns:
 *   A new string with no leading or trailing spaces/tabs/newlines.
 */


std::string toLower(std::string text) {
    std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return text;
}

/*
 * toLower
 * ----------------
 * Convert the input string to lower-case in-place and return it. Used
 * for simple case-insensitive comparisons (e.g. detecting clock/reset
 * names in port lists).
 *
 * Params:
 *   text - string to convert (copied by value so modification is safe)
 * Returns:
 *   Lower-cased string.
 */


std::string readFile(const std::filesystem::path &path) {
    std::ifstream ifs(path, std::ios::in | std::ios::binary);
    if (!ifs) {
        throw std::runtime_error("Unable to open file: " + path.string());
    }
    std::ostringstream oss;
    oss << ifs.rdbuf();
    return oss.str();
}

/*
 * readFile
 * ----------------
 * Read an entire file into a std::string. Throws a runtime_error if the
 * file cannot be opened. Binary-safe read is used to avoid platform
 * newline transformation that could change indices when parsing.
 *
 * Params:
 *   path - filesystem path to read
 * Returns:
 *   File contents as a single string.
 */


std::string stripComments(const std::string &text) {
    std::string result;
    result.reserve(text.size());
    enum class State { Normal, Slash, LineComment, BlockComment, BlockAsterisk };
    State state = State::Normal;

    for (char ch : text) {
        switch (state) {
        case State::Normal:
            if (ch == '/') {
                state = State::Slash;
            } else {
                result.push_back(ch);
            }
            break;
        case State::Slash:
            if (ch == '/') {
                state = State::LineComment;
                result.push_back(' ');
            } else if (ch == '*') {
                state = State::BlockComment;
                result.push_back(' ');
            } else {
                result.push_back('/');
                result.push_back(ch);
                state = State::Normal;
            }
            break;
        case State::LineComment:
            if (ch == '\n') {
                result.push_back('\n');
                state = State::Normal;
            }
            break;
        case State::BlockComment:
            if (ch == '*') {
                state = State::BlockAsterisk;
            }
            break;
        case State::BlockAsterisk:
            if (ch == '/') {
                state = State::Normal;
            } else if (ch != '*') {
                state = State::BlockComment;
            }
            break;
        }
    }

    if (state == State::Slash) {
        result.push_back('/');
    }

    return result;
}

/*
 * stripComments
 * ----------------
 * Remove both line (// ...) and block (/* ... * /) comments from the
 * provided SystemVerilog text. This implements a small finite-state
 * machine to preserve non-comment text and newlines while discarding
 * comments. The algorithm is intentionally conservative to avoid
 * removing character sequences that are not comments.
 *
 * Params:
 *   text - input SystemVerilog source
 * Returns:
 *   Source with comments removed (newlines preserved for line comments).
 */


size_t findMatchingParen(const std::string &text, size_t start) {
    int depth = 0;
    for (size_t idx = start; idx < text.size(); ++idx) {
        const char ch = text[idx];
        if (ch == '(') {
            ++depth;
        } else if (ch == ')') {
            --depth;
            if (depth == 0) {
                return idx;
            }
        }
    }
    throw std::runtime_error("Unbalanced parentheses in module header");
}

/*
 * findMatchingParen
 * ----------------
 * Find the index of the matching ')' for an opening '(' at position
 * `start` in the provided text. This counts nested parentheses to
 * correctly handle constructs such as parameter lists or nested
 * parentheses in port declarations.
 *
 * Params:
 *   text  - the input string containing parentheses
 *   start - index of the opening '(' to match
 * Returns:
 *   Index of the matching closing ')'. Throws runtime_error if not found.
 */


std::string extractPortRegion(const std::string &text, const std::string &moduleName) {
    const std::regex moduleRegex("\\bmodule\\s+" + escapeRegex(moduleName) + "\\b");
    std::smatch match;
    if (!std::regex_search(text, match, moduleRegex)) {
        throw std::runtime_error("Module '" + moduleName + "' not found.");
    }

    size_t cursor = match.position() + match.length();
    while (cursor < text.size() && std::isspace(static_cast<unsigned char>(text[cursor]))) {
        ++cursor;
    }

    if (cursor < text.size() && text[cursor] == '#') {
        const auto paramOpen = text.find('(', cursor);
        if (paramOpen == std::string::npos) {
            throw std::runtime_error("Malformed parameter block for module '" + moduleName + "'.");
        }
        cursor = findMatchingParen(text, paramOpen) + 1;
        while (cursor < text.size() && std::isspace(static_cast<unsigned char>(text[cursor]))) {
            ++cursor;
        }
    }

    const auto openParen = text.find('(', cursor);
    if (openParen == std::string::npos) {
        throw std::runtime_error("Unable to locate port list for module '" + moduleName + "'.");
    }
    const auto closingParen = findMatchingParen(text, openParen);
    return text.substr(openParen + 1, closingParen - openParen - 1);
}

/*
 * extractPortRegion
 * ----------------
 * Locate the module declaration for `moduleName` and extract the text
 * inside its primary port-parentheses '(... )'. Handles optional
 * parameter blocks after the module name (e.g. `#(parameter ...)`).
 * This function relies on `findMatchingParen` to correctly balance
 * parentheses and returns the substring containing the comma-separated
 * port header entries.
 *
 * Params:
 *   text       - full Verilog/SystemVerilog source (comments should be removed)
 *   moduleName - exact module identifier to find
 * Returns:
 *   Substring with the content between the module's '(' and matching ')'.
 * Throws:
 *   runtime_error if the module or port-list cannot be located or is malformed.
 */


std::vector<std::string> splitPortEntries(const std::string &section) {
    std::vector<std::string> entries;
    std::string current;
    int depth = 0;
    for (char ch : section) {
        if (ch == '(') {
            ++depth;
            current.push_back(ch);
            continue;
        }
        if (ch == ')') {
            --depth;
            current.push_back(ch);
            continue;
        }
        if (ch == ',' && depth == 0) {
            entries.push_back(trim(current));
            current.clear();
        } else {
            current.push_back(ch);
        }
    }
    if (!trim(current).empty()) {
        entries.push_back(trim(current));
    }
    return entries;
}

/*
 * splitPortEntries
 * ----------------
 * Split the module port-region string (the text between '(' and ')')
 * into individual entries separated by commas at depth zero. Commas
 * within nested parentheses or brackets are ignored (depth accounting).
 *
 * Params:
 *   section - port-list text to split
 * Returns:
 *   Vector of trimmed string entries representing each port declaration.
 */


std::vector<std::string> tokenize(const std::string &entry) {
    std::vector<std::string> tokens;
    std::string current;
    bool inBracket = false;
    for (char ch : entry) {
        if (std::isspace(static_cast<unsigned char>(ch)) && !inBracket) {
            if (!current.empty()) {
                tokens.push_back(current);
                current.clear();
            }
            continue;
        }
        if (ch == '[') {
            inBracket = true;
        } else if (ch == ']') {
            inBracket = false;
        }
        current.push_back(ch);
    }
    if (!current.empty()) {
        tokens.push_back(current);
    }
    return tokens;
}

/*
 * tokenize
 * ----------------
 * Tokenize a single port entry into whitespace-separated tokens while
 * treating bracketed widths (e.g. [7:0]) as single tokens. This keeps
 * bit-range expressions together so they can be reattached to types.
 *
 * Params:
 *   entry - a single comma-delimited port declaration
 * Returns:
 *   Vector of tokens for higher-level parsing by makePort().
 */


Port makePort(const std::string &entry, std::string &lastDirection, std::string &lastType) {
    auto effectiveEntry = entry;
    const auto assignPos = effectiveEntry.find('=');
    if (assignPos != std::string::npos) {
        effectiveEntry = effectiveEntry.substr(0, assignPos);
    }
    auto tokens = tokenize(effectiveEntry);
    Port port;
    port.direction = lastDirection;
    port.datatype = lastType;

    std::vector<std::string> leftovers;
    for (const auto &token : tokens) {
        const auto lowerToken = toLower(token);
        if (lowerToken == "input" || lowerToken == "output" || lowerToken == "inout") {
            port.direction = lowerToken;
            lastDirection = port.direction;
            continue;
        }
        if (token == "signed" || token == "unsigned") {
            leftovers.push_back(token);
            continue;
        }
        leftovers.push_back(token);
    }

    if (leftovers.empty()) {
        throw std::runtime_error("Unable to parse port entry: " + entry);
    }

    const std::string nameToken = leftovers.back();
    leftovers.pop_back();
    port.name = trim(nameToken);

    std::ostringstream typeStream;
    for (size_t idx = 0; idx < leftovers.size(); ++idx) {
        typeStream << leftovers[idx];
        if (idx + 1 < leftovers.size()) {
            typeStream << ' ';
        }
    }
    auto typeCandidate = trim(typeStream.str());
    if (typeCandidate.empty()) {
        typeCandidate = lastType;
    }
    if (typeCandidate.empty()) {
        typeCandidate = "logic";
    }
    if (!typeCandidate.empty() && typeCandidate.front() == '[') {
        typeCandidate = "logic " + typeCandidate;
    }
    port.datatype = trim(typeCandidate);
    lastType = port.datatype;

    if (port.direction.empty()) {
        throw std::runtime_error("Direction missing for port: " + port.name);
    }

    return port;
}

/*
 * makePort
 * ----------------
 * Parse a single port entry token list and construct a `Port` record.
 * This supports the common ANSI-style declarations used in SystemVerilog
 * module headers such as:
 *   input logic [7:0] data,
 *   output reg ready = 1'b0,
 *
 * The function keeps `lastDirection` and `lastType` to support elided
 * direction/type tokens (e.g. "input a, b;" where b inherits the
 * previous direction/type). Initialization expressions after '=' are
 * stripped.
 *
 * Params:
 *   entry         - raw port declaration (single entry)
 *   lastDirection - reference holding the previously seen direction
 *   lastType      - reference holding the previously seen type
 * Returns:
 *   Port structure with direction, datatype and name filled in.
 * Throws:
 *   runtime_error if parsing fails or direction is missing.
 */


std::vector<Port> parsePorts(const std::string &section) {
    const auto entries = splitPortEntries(section);
    std::vector<Port> ports;
    std::string lastDirection;
    std::string lastType;
    for (const auto &entry : entries) {
        if (entry.empty()) {
            continue;
        }
        ports.push_back(makePort(entry, lastDirection, lastType));
    }
    return ports;
}

/*
 * parsePorts
 * ----------------
 * Top-level port parser which splits the section into entries and
 * converts each into a `Port` using makePort. Tracks the last seen
 * direction/type so port lists that omit repeated keywords are parsed
 * correctly.
 *
 * Params:
 *   section - comma-separated port-list text
 * Returns:
 *   Vector of parsed Port structs.
 */


std::optional<std::string> detectClock(const std::vector<Port> &ports) {
    for (const auto &port : ports) {
        if (port.direction != "input" && port.direction != "inout") {
            continue;
        }
        const auto nameLower = toLower(port.name);
        if (nameLower.find("clk") != std::string::npos || nameLower.find("clock") != std::string::npos) {
            return port.name;
        }
    }
    return std::nullopt;
}

/*
 * detectClock
 * ----------------
 * Heuristic search of the parsed ports to find a clock signal name.
 * It looks for typical substrings such as "clk" or "clock" in input
 * port names and returns the first match. This is a simple heuristic
 * intended to make the generated driver/monitor use a sensible clock
 * edge for synchronization; it is not a replacement for explicit
 * configuration.
 *
 * Params:
 *   ports - parsed port list
 * Returns:
 *   Optional string with clock port name if found.
 */


std::pair<std::optional<std::string>, bool> detectReset(const std::vector<Port> &ports) {
    for (const auto &port : ports) {
        if (port.direction != "input" && port.direction != "inout") {
            continue;
        }
        const auto nameLower = toLower(port.name);
        if (nameLower.find("rst") != std::string::npos || nameLower.find("reset") != std::string::npos) {
            const bool activeLow = nameLower.find("n") != std::string::npos || nameLower.find("_b") != std::string::npos;
            return {port.name, activeLow};
        }
    }
    return {std::nullopt, false};
}

/*
 * detectReset
 * ----------------
 * Heuristic detection for reset-like signals. Looks for names
 * containing "rst" or "reset" and returns the name plus a bool
 * indicating whether the naming suggests active-low behavior (e.g.
 * contains 'n' or suffix '_b'). This helps generate the initial
 * reset stimulus (assert then deassert) with correct polarity.
 *
 * Params:
 *   ports - parsed port list
 * Returns:
 *   pair(optional reset name, activeLow flag)
 */


std::vector<Port> getDrivenPorts(const ModuleContext &ctx) {
    std::vector<Port> driven;
    for (const auto &port : ctx.ports) {
        if (port.direction == "input" || port.direction == "inout") {
            if (ctx.clockSignal && port.name == ctx.clockSignal.value()) {
                continue;
            }
            if (ctx.resetSignal && port.name == ctx.resetSignal.value()) {
                continue;
            }
            driven.push_back(port);
        }
    }
    return driven;
}

/*
 * getDrivenPorts
 * ----------------
 * From the module context, return the list of ports that are inputs
 * (or inout) and are candidates for being driven by the driver.
 * Clock and reset ports are excluded (they are handled separately).
 *
 * Params:
 *   ctx - module context containing parsed ports and detected signals
 * Returns:
 *   Vector of Port objects that should be driven by the testbench.
 */


std::vector<Port> getObservedPorts(const ModuleContext &ctx) {
    std::vector<Port> observed;
    for (const auto &port : ctx.ports) {
        if (port.direction == "output" || port.direction == "inout") {
            if (ctx.resetSignal && port.name == ctx.resetSignal.value()) {
                continue;
            }
            observed.push_back(port);
        }
    }
    return observed;
}

/*
 * getObservedPorts
 * ----------------
 * Return ports that are outputs (or inout) and should be sampled by
 * the monitor. Reset port is excluded to avoid noisy observations of
 * control signals.
 *
 * Params:
 *   ctx - module context
 * Returns:
 *   Vector of observable Port objects.
 */


std::string guardName(const std::string &moduleName, const std::string &suffix) {
    std::string guard;
    guard.reserve(moduleName.size() + suffix.size() + 4);
    for (char ch : moduleName) {
        if (std::isalnum(static_cast<unsigned char>(ch))) {
            guard.push_back(static_cast<char>(std::toupper(ch)));
        } else {
            guard.push_back('_');
        }
    }
    guard += '_';
    for (char ch : suffix) {
        if (std::isalnum(static_cast<unsigned char>(ch))) {
            guard.push_back(static_cast<char>(std::toupper(ch)));
        } else {
            guard.push_back('_');
        }
    }
    guard += "_SV";
    return guard;
}

/*
 * guardName
 * ----------------
 * Build a header-guard style identifier for generated SV files. This
 * produces an uppercase identifier based on the module name and a
 * suffix, replacing non-alphanumeric characters with underscores.
 *
 * Params:
 *   moduleName - source module name
 *   suffix     - additional suffix (e.g. "pkg" or "if")
 * Returns:
 *   Upper-cased guard identifier string.
 */


std::string generateInterface(const ModuleContext &ctx) {
    std::ostringstream oss;
    const auto guard = guardName(ctx.moduleName, "if");
    oss << "`ifndef " << guard << "\n";
    oss << "`define " << guard << "\n\n";
    oss << "interface " << ctx.moduleName << "_if;\n";
    for (const auto &port : ctx.ports) {
        oss << "  " << port.datatype << " " << port.name << ";\n";
    }
    oss << "endinterface : " << ctx.moduleName << "_if\n\n";
    oss << "`endif // " << guard << "\n";
    return oss.str();
}

/*
 * generateInterface
 * ----------------
 * Generate a SystemVerilog interface file string that declares an
 * interface containing one field per module port. The generator does
 * not attempt to create clocking blocks or modports; it emits a simple
 * interface that mirrors port names/types so the testbench can bind
 * signals to the DUT instance.
 *
 * Params:
 *   ctx - ModuleContext containing parsed ports and module name
 * Returns:
 *   Text content of the interface .sv file.
 */


std::string generateSeqItem(const ModuleContext &ctx) {
    const auto driven = getDrivenPorts(ctx);
    std::ostringstream oss;
    const auto className = ctx.moduleName + "_seq_item";
    oss << "class " << className << " extends uvm_sequence_item;\n";
    oss << "  `uvm_object_utils(" << className << ")\n";
    if (driven.empty()) {
        oss << "  // No randomizable ports were detected. Extend this item if needed.\n";
    }
    for (const auto &port : driven) {
        oss << "  rand " << port.datatype << " " << port.name << ";\n";
    }
    oss << "\n  function new(string name = \"" << className << "\");\n";
    oss << "    super.new(name);\n";
    oss << "  endfunction\n";
    oss << "endclass : " << className << "\n\n";
    return oss.str();
}

/*
 * generateSeqItem
 * ----------------
 * Create a UVM `uvm_sequence_item` class that contains randomizable
 * fields corresponding to the driven input ports of the DUT. The
 * resulting string is intended to be placed in the generated package.
 *
 * Params:
 *   ctx - module context with parsed ports
 * Returns:
 *   SV class definition text for the sequence item.
 */


std::string generateMonItem(const ModuleContext &ctx) {
    const auto observed = getObservedPorts(ctx);
    const auto className = ctx.moduleName + "_mon_item";
    std::ostringstream oss;
    oss << "class " << className << " extends uvm_sequence_item;\n";
    oss << "  `uvm_object_utils(" << className << ")\n";
    if (observed.empty()) {
        oss << "  // No observable ports were detected. Extend this item if needed.\n";
    }
    for (const auto &port : observed) {
        oss << "  " << port.datatype << " " << port.name << ";\n";
    }
    oss << "\n  function new(string name = \"" << className << "\");\n";
    oss << "    super.new(name);\n";
    oss << "  endfunction\n";
    oss << "endclass : " << className << "\n\n";
    return oss.str();
}

/*
 * generateMonItem
 * ----------------
 * Create a light-weight UVM sequence_item used by the monitor to
 * transport observed outputs to the scoreboard. It mirrors the
 * observed output port names/types.
 *
 * Params:
 *   ctx - module context
 * Returns:
 *   SV class definition for the monitor transaction item.
 */


std::string generateSequence(const ModuleContext &ctx) {
    const auto seqItem = ctx.moduleName + "_seq_item";
    const auto className = ctx.moduleName + "_sequence";
    std::ostringstream oss;
    oss << "class " << className << " extends uvm_sequence #(" << seqItem << ");\n";
    oss << "  `uvm_object_utils(" << className << ")\n";
    oss << "  rand int unsigned num_transactions = 10;\n";
    oss << "\n  function new(string name = \"" << className << "\");\n";
    oss << "    super.new(name);\n";
    oss << "  endfunction\n\n";
    oss << "  virtual task body();\n";
    oss << "    " << seqItem << " req;\n";
    oss << "    repeat (num_transactions) begin\n";
    oss << "      req = " << seqItem << "::type_id::create(\"req\");\n";
    oss << "      start_item(req);\n";
    oss << "      if (!req.randomize()) begin\n";
    oss << "        `uvm_error(\"RAND\", \"Sequence item randomization failed\")\n";
    oss << "      end\n";
    oss << "      finish_item(req);\n";
    oss << "    end\n";
    oss << "  endtask\n";
    oss << "endclass : " << className << "\n\n";
    return oss.str();
}

/*
 * generateSequence
 * ----------------
 * Create a simple UVM sequence that repeatedly generates randomized
 * sequence items and drives them to the DUT via the sequencer/driver.
 * This default sequence uses a `num_transactions` field to control
 * how many items are generated; users can modify the generated file
 * to add protocol-specific behavior.
 *
 * Params:
 *   ctx - module context
 * Returns:
 *   SV class definition for the sequence.
 */


std::string generateDriver(const ModuleContext &ctx) {
    const auto seqItem = ctx.moduleName + "_seq_item";
    const auto className = ctx.moduleName + "_driver";
    std::ostringstream oss;
    oss << "class " << className << " extends uvm_driver #(" << seqItem << ");\n";
    oss << "  `uvm_component_utils(" << className << ")\n";
    oss << "  virtual " << ctx.moduleName << "_if vif;\n";
    oss << "\n  function new(string name, uvm_component parent);\n";
    oss << "    super.new(name, parent);\n";
    oss << "  endfunction\n\n";
    oss << "  function void build_phase(uvm_phase phase);\n";
    oss << "    super.build_phase(phase);\n";
    oss << "    if (!uvm_config_db#(virtual " << ctx.moduleName << "_if)::get(this, \"\", \"vif\", vif)) begin\n";
    oss << "      `uvm_fatal(\"NOVIF\", \"Virtual interface not set for driver\")\n";
    oss << "    end\n";
    oss << "  endfunction\n\n";
    oss << "  task run_phase(uvm_phase phase);\n";
    oss << "    super.run_phase(phase);\n";
    oss << "    " << seqItem << " req;\n";
    oss << "    forever begin\n";
    oss << "      seq_item_port.get_next_item(req);\n";
    oss << "      drive_item(req);\n";
    oss << "      seq_item_port.item_done();\n";
    oss << "    end\n";
    oss << "  endtask\n\n";
    oss << "  task drive_item(" << seqItem << " item);\n";
    if (ctx.clockSignal) {
        oss << "    @(posedge vif." << ctx.clockSignal.value() << ");\n";
    } else {
        oss << "    #1;\n";
    }
    const auto driven = getDrivenPorts(ctx);
    if (driven.empty()) {
        oss << "    // Nothing to drive. Provide custom behavior here.\n";
    }
    for (const auto &port : driven) {
        oss << "    vif." << port.name << " <= item." << port.name << ";\n";
    }
    oss << "  endtask\n";
    oss << "endclass : " << className << "\n\n";
    return oss.str();
}

/*
 * generateDriver
 * ----------------
 * Produce the UVM driver implementation string. The driver gets a
 * virtual interface from the config DB, receives sequence items from
 * the sequencer, and applies values onto the virtual interface
 * signals. If a clock was detected, the driver waits for the positive
 * edge of the clock when driving; otherwise it uses a simple delay.
 *
 * Params:
 *   ctx - module context (used to find driven ports and clock signal)
 * Returns:
 *   SV class definition for the driver component.
 */


std::string generateMonitor(const ModuleContext &ctx) {
    const auto className = ctx.moduleName + "_monitor";
    const auto monItem = ctx.moduleName + "_mon_item";
    std::ostringstream oss;
    oss << "class " << className << " extends uvm_component;\n";
    oss << "  `uvm_component_utils(" << className << ")\n";
    oss << "  virtual " << ctx.moduleName << "_if vif;\n";
    oss << "  uvm_analysis_port #(" << monItem << ") ap;\n";
    oss << "\n  function new(string name, uvm_component parent);\n";
    oss << "    super.new(name, parent);\n";
    oss << "    ap = new(\"ap\", this);\n";
    oss << "  endfunction\n\n";
    oss << "  function void build_phase(uvm_phase phase);\n";
    oss << "    super.build_phase(phase);\n";
    oss << "    if (!uvm_config_db#(virtual " << ctx.moduleName << "_if)::get(this, \"\", \"vif\", vif)) begin\n";
    oss << "      `uvm_fatal(\"NOVIF\", \"Virtual interface not set for monitor\")\n";
    oss << "    end\n";
    oss << "  endfunction\n\n";
    oss << "  task run_phase(uvm_phase phase);\n";
    oss << "    super.run_phase(phase);\n";
    const auto observed = getObservedPorts(ctx);
    oss << "    forever begin\n";
    if (ctx.clockSignal) {
        oss << "      @(posedge vif." << ctx.clockSignal.value() << ");\n";
    } else {
        oss << "      #1;\n";
    }
    if (observed.empty()) {
        oss << "      // Nothing to sample. Add custom observation logic.\n";
        oss << "      ap.write(null);\n";
    } else {
        oss << "      " << monItem << " item = " << monItem << "::type_id::create(\"mon_item\");\n";
        for (const auto &port : observed) {
            oss << "      item." << port.name << " = vif." << port.name << ";\n";
        }
        oss << "      ap.write(item);\n";
    }
    oss << "    end\n";
    oss << "  endtask\n";
    oss << "endclass : " << className << "\n\n";
    return oss.str();
}

/*
 * generateMonitor
 * ----------------
 * Produce a UVM monitor that samples DUT outputs (or inouts) on the
 * detected clock edge (or simple delay if no clock detected) and
 * forwards these samples via an analysis port to the scoreboard.
 * The monitor also grabs the virtual interface from the config DB.
 *
 * Params:
 *   ctx - module context (used to find observed ports and clock)
 * Returns:
 *   SV class definition for the monitor component.
 */


std::string generateSequencer(const ModuleContext &ctx) {
    const auto seqItem = ctx.moduleName + "_seq_item";
    const auto className = ctx.moduleName + "_sequencer";
    std::ostringstream oss;
    oss << "class " << className << " extends uvm_sequencer #(" << seqItem << ");\n";
    oss << "  `uvm_component_utils(" << className << ")\n";
    oss << "\n  function new(string name, uvm_component parent);\n";
    oss << "    super.new(name, parent);\n";
    oss << "  endfunction\n";
    oss << "endclass : " << className << "\n\n";
    return oss.str();
}

/*
 * generateSequencer
 * ----------------
 * Emit a thin UVM sequencer class bound to the sequence item type. The
 * sequencer coordinates the flow of sequence items to the driver.
 *
 * Params:
 *   ctx - module context (used to form class names)
 * Returns:
 *   SV class definition for the sequencer.
 */


std::string generateAgent(const ModuleContext &ctx) {
    const auto className = ctx.moduleName + "_agent";
    const auto sequencer = ctx.moduleName + "_sequencer";
    const auto driver = ctx.moduleName + "_driver";
    const auto monitor = ctx.moduleName + "_monitor";
    std::ostringstream oss;
    oss << "class " << className << " extends uvm_agent;\n";
    oss << "  `uvm_component_utils(" << className << ")\n";
    oss << "  " << sequencer << " seqr;\n";
    oss << "  " << driver << " drv;\n";
    oss << "  " << monitor << " mon;\n";
    oss << "\n  function new(string name, uvm_component parent);\n";
    oss << "    super.new(name, parent);\n";
    oss << "  endfunction\n\n";
    oss << "  function void build_phase(uvm_phase phase);\n";
    oss << "    super.build_phase(phase);\n";
    oss << "    seqr = " << sequencer << "::type_id::create(\"seqr\", this);\n";
    oss << "    drv  = " << driver << "::type_id::create(\"drv\", this);\n";
    oss << "    mon  = " << monitor << "::type_id::create(\"mon\", this);\n";
    oss << "  endfunction\n\n";
    oss << "  function void connect_phase(uvm_phase phase);\n";
    oss << "    super.connect_phase(phase);\n";
    oss << "    drv.seq_item_port.connect(seqr.seq_item_export);\n";
    oss << "  endfunction\n";
    oss << "endclass : " << className << "\n\n";
    return oss.str();
}

/*
 * generateAgent
 * ----------------
 * Generate a UVM agent that instantiates the sequencer, driver and
 * monitor and performs the necessary connect operations in the
 * connect_phase. The agent is a single-place holder that composes
 * those components for use inside the environment.
 *
 * Params:
 *   ctx - module context
 * Returns:
 *   SV class definition for the UVM agent.
 */


std::string generateScoreboard(const ModuleContext &ctx) {
    const auto className = ctx.moduleName + "_scoreboard";
    const auto monItem = ctx.moduleName + "_mon_item";
    std::ostringstream oss;
    oss << "class " << className << " extends uvm_component;\n";
    oss << "  `uvm_component_utils(" << className << ")\n";
    oss << "  uvm_analysis_imp #(" << monItem << ", " << className << ") mon_imp;\n";
    oss << "\n  function new(string name, uvm_component parent);\n";
    oss << "    super.new(name, parent);\n";
    oss << "    mon_imp = new(\"mon_imp\", this);\n";
    oss << "  endfunction\n\n";
    oss << "  virtual function void write(" << monItem << " t);\n";
    oss << "    if (t == null) begin\n";
    oss << "      `uvm_warning(\"EMPTY\", \"Monitor transaction is null\")\n";
    oss << "      return;\n";
    oss << "    end\n";
    oss << "    `uvm_info(\"SCOREBOARD\", t.sprint(), UVM_LOW)\n";
    oss << "  endfunction\n";
    oss << "endclass : " << className << "\n\n";
    return oss.str();
}

/*
 * generateScoreboard
 * ----------------
 * Emit a very small scoreboard component that implements an
 * `uvm_analysis_imp` to receive monitor transactions. The default
 * implementation simply logs the transaction using `uvm_info`.
 *
 * Params:
 *   ctx - module context
 * Returns:
 *   SV class definition for the scoreboard component.
 */


std::string generateEnv(const ModuleContext &ctx) {
    const auto className = ctx.moduleName + "_env";
    const auto agent = ctx.moduleName + "_agent";
    const auto scoreboard = ctx.moduleName + "_scoreboard";
    std::ostringstream oss;
    oss << "class " << className << " extends uvm_env;\n";
    oss << "  `uvm_component_utils(" << className << ")\n";
    oss << "  " << agent << " agent;\n";
    oss << "  " << scoreboard << " scb;\n";
    oss << "  virtual " << ctx.moduleName << "_if vif;\n";
    oss << "\n  function new(string name, uvm_component parent);\n";
    oss << "    super.new(name, parent);\n";
    oss << "  endfunction\n\n";
    oss << "  function void build_phase(uvm_phase phase);\n";
    oss << "    super.build_phase(phase);\n";
    oss << "    if (!uvm_config_db#(virtual " << ctx.moduleName << "_if)::get(this, \"\", \"vif\", vif)) begin\n";
    oss << "      `uvm_fatal(\"NOVIF\", \"Virtual interface not provided to env\")\n";
    oss << "    end\n";
    oss << "    agent = " << agent << "::type_id::create(\"agent\", this);\n";
    oss << "    scb = " << scoreboard << "::type_id::create(\"scb\", this);\n";
    oss << "    uvm_config_db#(virtual " << ctx.moduleName << "_if)::set(this, \"agent.*\", \"vif\", vif);\n";
    oss << "  endfunction\n\n";
    oss << "  function void connect_phase(uvm_phase phase);\n";
    oss << "    super.connect_phase(phase);\n";
    oss << "    agent.mon.ap.connect(scb.mon_imp);\n";
    oss << "  endfunction\n";
    oss << "endclass : " << className << "\n\n";
    return oss.str();
}

/*
 * generateEnv
 * ----------------
 * Create a top-level environment which instantiates the agent and
 * scoreboard, wires the virtual interface through the config DB, and
 * connects the monitor analysis port to the scoreboard implementation.
 *
 * Params:
 *   ctx - module context
 * Returns:
 *   SV class definition for the environment.
 */


std::string generateTest(const ModuleContext &ctx) {
    const auto className = ctx.moduleName + "_test";
    const auto env = ctx.moduleName + "_env";
    const auto sequence = ctx.moduleName + "_sequence";
    std::ostringstream oss;
    oss << "class " << className << " extends uvm_test;\n";
    oss << "  `uvm_component_utils(" << className << ")\n";
    oss << "  " << env << " env;\n";
    oss << "  " << sequence << " seq;\n";
    oss << "\n  function new(string name, uvm_component parent);\n";
    oss << "    super.new(name, parent);\n";
    oss << "  endfunction\n\n";
    oss << "  function void build_phase(uvm_phase phase);\n";
    oss << "    super.build_phase(phase);\n";
    oss << "    env = " << env << "::type_id::create(\"env\", this);\n";
    oss << "    seq = " << sequence << "::type_id::create(\"seq\");\n";
    oss << "  endfunction\n\n";
    oss << "  task run_phase(uvm_phase phase);\n";
    oss << "    phase.raise_objection(this);\n";
    oss << "    seq.start(env.agent.seqr);\n";
    oss << "    phase.drop_objection(this);\n";
    oss << "  endtask\n";
    oss << "endclass : " << className << "\n\n";
    return oss.str();
}

/*
 * generateTest
 * ----------------
 * Construct a simple top-level UVM test which creates the environment
 * and starts the default sequence. The test raises/drops objections to
 * control simulation lifetime in the UVM standard way.
 *
 * Params:
 *   ctx - module context
 * Returns:
 *   SV class definition for the test.
 */


std::string generatePackage(const ModuleContext &ctx) {
    std::ostringstream oss;
    const auto guard = guardName(ctx.moduleName, "pkg");
    oss << "`ifndef " << guard << "\n";
    oss << "`define " << guard << "\n\n";
    oss << "package " << ctx.moduleName << "_pkg;\n";
    oss << "  import uvm_pkg::*;\n";
    oss << "  `include \"uvm_macros.svh\"\n\n";
    oss << generateSeqItem(ctx);
    oss << generateMonItem(ctx);
    oss << generateSequence(ctx);
    oss << generateSequencer(ctx);
    oss << generateDriver(ctx);
    oss << generateMonitor(ctx);
    oss << generateAgent(ctx);
    oss << generateScoreboard(ctx);
    oss << generateEnv(ctx);
    oss << generateTest(ctx);
    oss << "endpackage : " << ctx.moduleName << "_pkg\n\n";
    oss << "`endif // " << guard << "\n";
    return oss.str();
}

/*
 * generatePackage
 * ----------------
 * Aggregate all generated UVM classes into a single SystemVerilog
 * package. This helps consumers `import` the test components and keeps
 * the UVM definitions grouped. The package also receives a header
 * guard to prevent multiple inclusion.
 *
 * Params:
 *   ctx - module context
 * Returns:
 *   SV text for the package file containing all generated classes.
 */


std::string moduleInstance(const ModuleContext &ctx) {
    std::ostringstream oss;
    oss << ctx.moduleName << " dut (\n";
    for (size_t idx = 0; idx < ctx.ports.size(); ++idx) {
        const auto &port = ctx.ports[idx];
        oss << "    ." << port.name << "(dut_if." << port.name << ")";
        if (idx + 1 < ctx.ports.size()) {
            oss << ",";
        }
        oss << "\n";
    }
    oss << "  );\n";
    return oss.str();
}

/*
 * moduleInstance
 * ----------------
 * Produce a text snippet that instantiates the DUT and wires each
 * port to a similarly-named signal in the generated `dut_if` virtual
 * interface instance. This snippet is used inside the generated
 * testbench module.
 *
 * Params:
 *   ctx - module context
 * Returns:
 *   Multiline string containing DUT instantiation text.
 */


std::string generateInitializations(const ModuleContext &ctx) {
    std::ostringstream oss;
    const auto driven = getDrivenPorts(ctx);
    if (ctx.clockSignal) {
        oss << "  initial begin\n";
        oss << "    dut_if." << ctx.clockSignal.value() << " = 0;\n";
        oss << "    forever #5 dut_if." << ctx.clockSignal.value() << " = ~dut_if." << ctx.clockSignal.value() << ";\n";
        oss << "  end\n";
    }
    if (ctx.resetSignal) {
        oss << "  initial begin\n";
        oss << "    dut_if." << ctx.resetSignal.value() << " = " << (ctx.resetActiveLow ? '1' : '0') << ";\n";
        oss << "    #20;\n";
        oss << "    dut_if." << ctx.resetSignal.value() << " = " << (ctx.resetActiveLow ? '0' : '1') << ";\n";
        oss << "  end\n";
    }
    if (!driven.empty()) {
        oss << "  initial begin\n";
        for (const auto &port : driven) {
            oss << "    dut_if." << port.name << " = '0;\n";
        }
        oss << "  end\n";
    }
    return oss.str();
}

/*
 * generateInitializations
 * ----------------
 * Emit SV initial blocks for clock generation, reset sequencing and
 * default initial values for driven ports. Uses the detected clock
 * and reset names to produce a simple simulation-time sequence that
 * can be used to run the UVM test.
 *
 * Params:
 *   ctx - module context
 * Returns:
 *   SV text with initial blocks to be placed in the top-level testbench.
 */


std::string generateTestbench(const ModuleContext &ctx) {
    std::ostringstream oss;
    oss << "`timescale 1ns/1ps\n";
    oss << "`include \"uvm_macros.svh\"\n";
    oss << "`include \"" << ctx.moduleName << "_if.sv\"\n";
    oss << "`include \"" << ctx.moduleName << "_pkg.sv\"\n\n";
    oss << "module tb_" << ctx.moduleName << ";\n";
    oss << "  import uvm_pkg::*;\n";
    oss << "  import " << ctx.moduleName << "_pkg::*;\n\n";
    oss << "  " << ctx.moduleName << "_if dut_if();\n";
    oss << "  " << moduleInstance(ctx) << "\n";
    oss << generateInitializations(ctx);
    oss << "\n  initial begin\n";
    oss << "    uvm_config_db#(virtual " << ctx.moduleName << "_if)::set(null, \"uvm_test_top.env\", \"vif\", dut_if);\n";
    oss << "    run_test(\"" << ctx.moduleName << "_test\");\n";
    oss << "  end\n";
    oss << "endmodule : tb_" << ctx.moduleName << "\n";
    return oss.str();
}

/*
 * generateTestbench
 * ----------------
 * Build the top-level testbench SV file including `timescale`, the
 * generated interface and package, a `dut_if` instance, DUT
 * instantiation, initializations, and a `run_test` invocation that
 * starts the UVM test.
 *
 * Params:
 *   ctx - module context
 * Returns:
 *   Complete SystemVerilog testbench source string.
 */


std::string quotePath(const std::filesystem::path &path) {
    return std::string("\"") + path.generic_string() + "\"";
}

/*
 * quotePath
 * ----------------
 * Utility to produce a quoted path string suitable for passing to
 * command-line tools in the generated .do script.
 *
 * Params:
 *   path - filesystem path to quote
 * Returns:
 *   Quoted path string.
 */


std::string generateDoScript(const ModuleContext &ctx, const std::filesystem::path &svDir, const std::filesystem::path &tbPath, const std::filesystem::path &dutPath) {
    const auto ifPath = svDir / (ctx.moduleName + "_if.sv");
    const auto pkgPath = svDir / (ctx.moduleName + "_pkg.sv");
    std::ostringstream oss;
    oss << "transcript on\n";
    oss << "vlib work\n";
    oss << "vmap work work\n";
    oss << "vlog +acc +cover=bcesf +incdir+" << svDir.generic_string() << " "
        << quotePath(ifPath) << " "
        << quotePath(pkgPath) << " "
        << quotePath(dutPath) << " "
        << quotePath(tbPath) << "\n";
    oss << "vsim -c work.tb_" << ctx.moduleName << " -do \"run -all; quit\"\n";
    return oss.str();
}

/*
 * generateDoScript
 * ----------------
 * Create a Mentor/Questa .do script that sets up a work library,
 * compiles the generated interface/package, the DUT (supplied by the
 * user), and the testbench, then runs the resulting test. The script
 * uses `vlog` with common coverage/acc options and then invokes `vsim`.
 *
 * Params:
 *   ctx    - module context
 *   svDir  - output directory for generated SV files
 *   tbPath - path to the generated testbench file
 *   dutPath- path to the original DUT source file
 * Returns:
 *   Text content for the .do script.
 */


void writeTextFile(const std::filesystem::path &path, const std::string &content) {
    std::ofstream ofs(path, std::ios::out | std::ios::trunc);
    if (!ofs) {
        throw std::runtime_error("Unable to write file: " + path.string());
    }
    ofs << content;
}

/*
 * writeTextFile
 * ----------------
 * Write `content` into `path`, truncating any previous contents.
 * Throws runtime_error on failure. This keeps the generator simple
 * and leaves error handling to the caller.
 *
 * Params:
 *   path    - output file path
 *   content - text to write
 */


} // namespace

int main() {
    /*
     * main
     * ----------------
     * Interactive entry point for the UVM generator utility. The program
     * prompts the user for a Verilog/SystemVerilog source path and a
     * module name. It then reads the file, strips comments, parses the
     * module port list (ANSI-style), detects a clock/reset if present,
     * and emits generated files under `uvm_<module>/sv/` plus a .do
     * script. Errors during parsing or IO are reported and cause a
     * non-zero exit.
     */
    try {
        std::string verilogPathInput;
        std::cout << "Enter path to the Verilog/SystemVerilog file of the Top Module: ";
        std::getline(std::cin, verilogPathInput);
        verilogPathInput = trim(verilogPathInput);
        if (verilogPathInput.empty()) {
            throw std::runtime_error("Verilog file path is required.");
        }

        std::string moduleName;
        std::cout << "Enter the DUT Top module name: ";
        std::getline(std::cin, moduleName);
        moduleName = trim(moduleName);
        if (moduleName.empty()) {
            throw std::runtime_error("Module name is required.");
        }

        const std::filesystem::path dutPath = std::filesystem::absolute(verilogPathInput);
        auto fileContent = readFile(dutPath);
        fileContent = stripComments(fileContent);
        ModuleContext ctx;
        ctx.moduleName = moduleName;
        ctx.dutSource = dutPath;
        const auto portSection = extractPortRegion(fileContent, moduleName);
        ctx.ports = parsePorts(portSection);
        ctx.clockSignal = detectClock(ctx.ports);
        const auto [resetName, activeLow] = detectReset(ctx.ports);
        ctx.resetSignal = resetName;
        ctx.resetActiveLow = activeLow;

        if (ctx.ports.empty()) {
            throw std::runtime_error("No ports were parsed from module header. Ensure ANSI-style port declarations.");
        }

        const auto outRoot = std::filesystem::current_path() / ("uvm_" + moduleName);
        const auto svDir = outRoot / "sv";
        std::filesystem::create_directories(svDir);

        const auto ifPath = svDir / (moduleName + "_if.sv");
        const auto pkgPath = svDir / (moduleName + "_pkg.sv");
        const auto tbPath = svDir / ("tb_" + moduleName + ".sv");
        const auto doPath = outRoot / ("run_" + moduleName + ".do");

        writeTextFile(ifPath, generateInterface(ctx));
        writeTextFile(pkgPath, generatePackage(ctx));
        writeTextFile(tbPath, generateTestbench(ctx));
        writeTextFile(doPath, generateDoScript(ctx, svDir, tbPath, dutPath));

        std::cout << "Generated UVM test suite under " << outRoot << "\n";
        std::cout << "Run Mentor/Questa with: vsim -do " << doPath << "\n";
    } catch (const std::exception &ex) {
        std::cerr << "Error: " << ex.what() << "\n";
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
