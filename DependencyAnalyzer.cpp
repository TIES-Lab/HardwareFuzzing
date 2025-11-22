#include <algorithm>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <optional>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

struct Port {
    std::string direction;
    std::string datatype;
    std::string name;
};

struct ModuleContext {
    std::string moduleName;
    std::vector<Port> ports;
};

struct Clause {
    std::string condition; // Empty means unconditional
    std::string expression;
};

using EquationMap = std::unordered_map<std::string, std::vector<Clause>>;

namespace {

// --------------------------
// Utility helpers
// --------------------------

std::string trim(const std::string &text) {
    const auto first = text.find_first_not_of(" \t\n\r");
    if (first == std::string::npos) {
        return "";
    }
    const auto last = text.find_last_not_of(" \t\n\r");
    return text.substr(first, last - first + 1);
}

std::string toLower(std::string text) {
    std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return text;
}

std::string readFile(const std::filesystem::path &path) {
    std::ifstream ifs(path, std::ios::in | std::ios::binary);
    if (!ifs) {
        throw std::runtime_error("Unable to open file: " + path.string());
    }
    std::ostringstream oss;
    oss << ifs.rdbuf();
    return oss.str();
}

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

size_t findMatchingParen(const std::string &text, size_t start) {
    int depth = 0;
    bool inString = false;
    for (size_t idx = start; idx < text.size(); ++idx) {
        const char ch = text[idx];
        if (ch == '"' && (idx == 0 || text[idx - 1] != '\\')) {
            inString = !inString;
            continue;
        }
        if (inString) {
            continue;
        }
        if (ch == '(') {
            ++depth;
        } else if (ch == ')') {
            --depth;
            if (depth == 0) {
                return idx;
            }
        }
    }
    throw std::runtime_error("Unbalanced parentheses detected.");
}

bool isWordBoundary(const std::string &text, size_t pos, size_t len) {
    const bool left = (pos == 0) || (!std::isalnum(static_cast<unsigned char>(text[pos - 1])) && text[pos - 1] != '_');
    const size_t after = pos + len;
    const bool right = (after >= text.size()) || (!std::isalnum(static_cast<unsigned char>(text[after])) && text[after] != '_');
    return left && right;
}

bool isAlwaysToken(const std::string &text, size_t pos) {
    if (text.compare(pos, 6, "always") != 0) {
        return false;
    }
    if (pos > 0) {
        const char prev = text[pos - 1];
        if (std::isalnum(static_cast<unsigned char>(prev)) || prev == '_') {
            return false;
        }
    }
    const size_t after = pos + 6;
    if (after >= text.size()) {
        return true;
    }
    const char next = text[after];
    if (next == '_' || next == ' ' || next == '\t' || next == '\n' || next == '@' || next == '(') {
        return true;
    }
    return !std::isalnum(static_cast<unsigned char>(next));
}

// --------------------------
// Module port parsing (re-used from generator)
// --------------------------

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
            throw std::runtime_error("Malformed parameter block near module header.");
        }
        cursor = findMatchingParen(text, paramOpen) + 1;
        while (cursor < text.size() && std::isspace(static_cast<unsigned char>(text[cursor]))) {
            ++cursor;
        }
    }

    const auto openParen = text.find('(', cursor);
    if (openParen == std::string::npos) {
        throw std::runtime_error("Port list not found for module header.");
    }
    const auto closingParen = findMatchingParen(text, openParen);
    return text.substr(openParen + 1, closingParen - openParen - 1);
}

std::vector<std::string> splitPortEntries(const std::string &section) {
    std::vector<std::string> entries;
    std::string current;
    int depth = 0;
    bool inString = false;
    for (char ch : section) {
        if (ch == '"' && (current.empty() || current.back() != '\\')) {
            inString = !inString;
        }
        if (inString) {
            current.push_back(ch);
            continue;
        }
        if (ch == '(') {
            ++depth;
        } else if (ch == ')') {
            --depth;
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
        typeCandidate = lastType.empty() ? "logic" : lastType;
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

// --------------------------
// Statement AST for block parsing
// --------------------------

struct Statement;
using StatementPtr = std::unique_ptr<Statement>;

struct CaseItem {
    std::vector<std::string> labels;
    StatementPtr body;
};

struct Statement {
    enum class Kind { Assignment, Block, If, Case } kind;
    std::string lhs;
    std::string rhs;
    std::string op;
    std::vector<StatementPtr> blockStatements;
    struct {
        std::string condition;
        StatementPtr thenBranch;
        StatementPtr elseBranch;
    } ifStmt;
    struct {
        std::string selector;
        std::vector<CaseItem> items;
    } caseStmt;
};

struct Assignment {
    std::string lhs;
    std::string op;
    std::string rhs;
};

std::optional<Assignment> parseAssignmentStatement(const std::string &statementText) {
    const auto trimmed = trim(statementText);
    if (trimmed.empty()) {
        return std::nullopt;
    }

    auto findOp = [&](const std::string &text) -> std::optional<std::pair<size_t, std::string>> {
        auto pos = text.find("<=");
        if (pos != std::string::npos) {
            return std::make_pair(pos, std::string("<="));
        }
        pos = text.find('=');
        while (pos != std::string::npos) {
            const char prev = pos > 0 ? text[pos - 1] : '\0';
            const char next = pos + 1 < text.size() ? text[pos + 1] : '\0';
            if (prev == '!' || prev == '=' || prev == '<' || prev == '>' || prev == ':' || prev == '&' || prev == '|') {
                pos = text.find('=', pos + 1);
                continue;
            }
            if (next == '=') {
                pos = text.find('=', pos + 1);
                continue;
            }
            return std::make_pair(pos, std::string("="));
        }
        return std::nullopt;
    };

    const auto opInfo = findOp(trimmed);
    if (!opInfo) {
        return std::nullopt;
    }
    const auto [opPos, opStr] = *opInfo;
    auto lhs = trim(trimmed.substr(0, opPos));
    auto rhs = trim(trimmed.substr(opPos + opStr.size()));
    if (!rhs.empty() && rhs.back() == ';') {
        rhs.pop_back();
    }
    rhs = trim(rhs);
    if (lhs.empty() || rhs.empty()) {
        return std::nullopt;
    }
    return Assignment{lhs, opStr, rhs};
}

class BlockParser {
  public:
    explicit BlockParser(std::string text) : text_(std::move(text)) {}

    std::vector<StatementPtr> parseStatements() {
        std::vector<StatementPtr> statements;
        while (true) {
            skipWhitespace();
            if (eof()) {
                break;
            }
            if (startsWith("end")) {
                break;
            }
            if (startsWith("endcase")) {
                break;
            }
            auto stmt = parseStatement();
            if (!stmt) {
                continue;
            }
            statements.push_back(std::move(stmt));
        }
        return statements;
    }

  private:
    const std::string text_;
    size_t pos_ = 0;

    bool eof() const { return pos_ >= text_.size(); }

    void skipWhitespace() {
        while (!eof()) {
            char ch = text_[pos_];
            if (std::isspace(static_cast<unsigned char>(ch))) {
                ++pos_;
                continue;
            }
            if (ch == '/' && pos_ + 1 < text_.size()) {
                if (text_[pos_ + 1] == '/') {
                    pos_ += 2;
                    while (!eof() && text_[pos_] != '\n') {
                        ++pos_;
                    }
                    continue;
                }
                if (text_[pos_ + 1] == '*') {
                    pos_ += 2;
                    while (!eof()) {
                        if (text_[pos_] == '*' && pos_ + 1 < text_.size() && text_[pos_ + 1] == '/') {
                            pos_ += 2;
                            break;
                        }
                        ++pos_;
                    }
                    continue;
                }
            }
            break;
        }
    }

    bool startsWith(const std::string &kw) {
        skipWhitespace();
        if (text_.compare(pos_, kw.size(), kw) != 0) {
            return false;
        }
        if (pos_ + kw.size() < text_.size()) {
            const char ch = text_[pos_ + kw.size()];
            if (std::isalnum(static_cast<unsigned char>(ch)) || ch == '_') {
                return false;
            }
        }
        return true;
    }

    bool consumeKeyword(const std::string &kw) {
        if (!startsWith(kw)) {
            return false;
        }
        pos_ += kw.size();
        return true;
    }

    bool dropPlainLine() {
        size_t scan = pos_;
        size_t lineEnd = scan;
        bool hasCode = false;
        while (lineEnd < text_.size() && text_[lineEnd] != '\n') {
            const char ch = text_[lineEnd];
            if (ch == '=' || ch == ';' || ch == '(' || ch == ')' || ch == '[' || ch == ']' || ch == ':' || ch == '.') {
                hasCode = true;
            }
            ++lineEnd;
        }
        if (!hasCode) {
            pos_ = (lineEnd < text_.size()) ? lineEnd + 1 : lineEnd;
            return true;
        }
        return false;
    }

    std::string readBalanced(char open, char close) {
        skipWhitespace();
        if (eof() || text_[pos_] != open) {
            throw std::runtime_error("Expected balanced segment starting with '" + std::string(1, open) + "'.");
        }
        size_t start = pos_ + 1;
        ++pos_;
        int depth = 1;
        bool inString = false;
        while (!eof()) {
            char ch = text_[pos_++];
            if (ch == '"' && (pos_ == 0 || text_[pos_ - 2] != '\\')) {
                inString = !inString;
                continue;
            }
            if (inString) {
                continue;
            }
            if (ch == open) {
                ++depth;
            } else if (ch == close) {
                --depth;
                if (depth == 0) {
                    break;
                }
            }
        }
        return trim(text_.substr(start, pos_ - start - 1));
    }

    std::string readUntil(char terminator) {
        skipWhitespace();
        size_t start = pos_;
        int depth = 0;
        bool inString = false;
        while (!eof()) {
            char ch = text_[pos_++];
            if (ch == '"' && (pos_ == 0 || text_[pos_ - 2] != '\\')) {
                inString = !inString;
                continue;
            }
            if (inString) {
                continue;
            }
            if (ch == '(') {
                ++depth;
            } else if (ch == ')') {
                --depth;
            } else if (ch == terminator && depth == 0) {
                break;
            }
        }
        return trim(text_.substr(start, pos_ - start));
    }

    StatementPtr parseStatement() {
        skipWhitespace();
        if (eof()) {
            return nullptr;
        }
        if (startsWith("if")) {
            return parseIf();
        }
        if (startsWith("case") || startsWith("casez") || startsWith("casex") || startsWith("unique case") || startsWith("priority case")) {
            return parseCase();
        }
        if (startsWith("begin")) {
            return parseBlock();
        }
        if (dropPlainLine()) {
            return nullptr;
        }
        auto raw = readUntil(';');
        auto assignment = parseAssignmentStatement(raw + ";");
        if (!assignment) {
            return nullptr;
        }
        auto stmt = std::make_unique<Statement>();
        stmt->kind = Statement::Kind::Assignment;
        stmt->lhs = assignment->lhs;
        stmt->rhs = assignment->rhs;
        stmt->op = assignment->op;
        return stmt;
    }

    StatementPtr parseBlock() {
        consumeKeyword("begin");
        auto block = std::make_unique<Statement>();
        block->kind = Statement::Kind::Block;
        while (true) {
            skipWhitespace();
            if (startsWith("end")) {
                consumeKeyword("end");
                break;
            }
            if (eof()) {
                break;
            }
            auto stmt = parseStatement();
            if (!stmt) {
                continue;
            }
            block->blockStatements.push_back(std::move(stmt));
        }
        return block;
    }

    StatementPtr parseIf() {
        consumeKeyword("if");
        auto condition = readBalanced('(', ')');
        auto thenBranch = parseStatement();
        StatementPtr elseBranch;
        skipWhitespace();
        if (startsWith("else")) {
            consumeKeyword("else");
            if (startsWith("if")) {
                elseBranch = parseIf();
            } else {
                elseBranch = parseStatement();
            }
        }
        auto stmt = std::make_unique<Statement>();
        stmt->kind = Statement::Kind::If;
        stmt->ifStmt.condition = condition;
        stmt->ifStmt.thenBranch = std::move(thenBranch);
        stmt->ifStmt.elseBranch = std::move(elseBranch);
        return stmt;
    }

    StatementPtr parseCase() {
        std::string keyword;
        if (consumeKeyword("unique case")) {
            keyword = "unique case";
        } else if (consumeKeyword("priority case")) {
            keyword = "priority case";
        } else if (startsWith("casez")) {
            keyword = "casez";
            consumeKeyword("casez");
        } else if (startsWith("casex")) {
            keyword = "casex";
            consumeKeyword("casex");
        } else {
            keyword = "case";
            consumeKeyword("case");
        }
        (void)keyword;
        auto selector = readBalanced('(', ')');
        auto stmt = std::make_unique<Statement>();
        stmt->kind = Statement::Kind::Case;
        stmt->caseStmt.selector = selector;

        while (true) {
            skipWhitespace();
            if (startsWith("endcase")) {
                consumeKeyword("endcase");
                break;
            }
            if (eof()) {
                break;
            }
            size_t labelStart = pos_;
            while (!eof()) {
                char ch = text_[pos_];
                if (ch == ':') {
                    break;
                }
                ++pos_;
            }
            if (eof()) {
                break;
            }
            const auto labelSegment = trim(text_.substr(labelStart, pos_ - labelStart));
            ++pos_; // skip ':'
            std::vector<std::string> labels;
            std::stringstream ss(labelSegment);
            std::string label;
            while (std::getline(ss, label, ',')) {
                const auto trimmedLabel = trim(label);
                if (!trimmedLabel.empty()) {
                    labels.push_back(trimmedLabel);
                }
            }
            auto body = parseStatement();
            stmt->caseStmt.items.push_back(CaseItem{labels, std::move(body)});
        }
        return stmt;
    }
};

std::string combineConditions(const std::string &a, const std::string &b) {
    if (a.empty()) {
        return b;
    }
    if (b.empty()) {
        return a;
    }
    return "(" + a + ") && (" + b + ")";
}

std::string sanitizeExpression(const std::string &expr) {
    auto cleaned = trim(expr);
    if (!cleaned.empty() && cleaned.back() == ';') {
        cleaned.pop_back();
        cleaned = trim(cleaned);
    }
    return cleaned;
}

std::string defaultOutputPath(const std::filesystem::path &rtlPath, const std::string &moduleName) {
    auto stem = rtlPath.stem().string();
    if (stem.empty()) {
        stem = moduleName;
    }
    return stem + "_dependencies.txt";
}

void collectAssignments(const Statement &stmt, const std::string &condition, EquationMap &equations) {
    switch (stmt.kind) {
    case Statement::Kind::Assignment: {
        equations[stmt.lhs].push_back(Clause{condition, sanitizeExpression(stmt.rhs)});
        break;
    }
    case Statement::Kind::Block: {
        for (const auto &child : stmt.blockStatements) {
            collectAssignments(*child, condition, equations);
        }
        break;
    }
    case Statement::Kind::If: {
        const auto cond = trim(stmt.ifStmt.condition);
        const auto thenCond = combineConditions(condition, cond);
        if (stmt.ifStmt.thenBranch) {
            collectAssignments(*stmt.ifStmt.thenBranch, thenCond, equations);
        }
        if (stmt.ifStmt.elseBranch) {
            const auto negated = combineConditions(condition, "!(" + cond + ")");
            collectAssignments(*stmt.ifStmt.elseBranch, negated, equations);
        }
        break;
    }
    case Statement::Kind::Case: {
        for (const auto &item : stmt.caseStmt.items) {
            if (!item.body) {
                continue;
            }
            std::vector<std::string> labelConds;
            for (const auto &label : item.labels) {
                if (toLower(label) == "default") {
                    labelConds.push_back("1'b1");
                } else {
                    labelConds.push_back("(" + stmt.caseStmt.selector + ") == (" + label + ")");
                }
            }
            std::string combinedLabels;
            if (!labelConds.empty()) {
                combinedLabels = labelConds.front();
                for (size_t idx = 1; idx < labelConds.size(); ++idx) {
                    combinedLabels = "(" + combinedLabels + ") || (" + labelConds[idx] + ")";
                }
            }
            const auto guard = combineConditions(condition, combinedLabels);
            collectAssignments(*item.body, guard, equations);
        }
        break;
    }
    }
}

void parseContinuousAssignments(const std::string &text, EquationMap &equations) {
    size_t pos = 0;
    while (true) {
        pos = text.find("assign", pos);
        if (pos == std::string::npos) {
            break;
        }
        if (!isWordBoundary(text, pos, 6)) {
            pos += 6;
            continue;
        }
        size_t stmtStart = pos + 6;
        while (stmtStart < text.size() && std::isspace(static_cast<unsigned char>(text[stmtStart]))) {
            ++stmtStart;
        }
        size_t cursor = stmtStart;
        int depth = 0;
        bool inString = false;
        while (cursor < text.size()) {
            char ch = text[cursor++];
            if (ch == '"' && (cursor < 2 || text[cursor - 2] != '\\')) {
                inString = !inString;
                continue;
            }
            if (inString) {
                continue;
            }
            if (ch == '(') {
                ++depth;
            } else if (ch == ')') {
                --depth;
            } else if (ch == ';' && depth == 0) {
                break;
            }
        }
        const auto statement = trim(text.substr(stmtStart, cursor - stmtStart - 1));
        auto assignment = parseAssignmentStatement(statement + ";");
        if (assignment) {
                equations[assignment->lhs].push_back(Clause{"", sanitizeExpression(assignment->rhs)});
        }
        pos = cursor;
    }
}

size_t findMatchingEnd(const std::string &text, size_t beginPos) {
    size_t pos = beginPos;
    int depth = 0;
    while (pos < text.size()) {
        if (text.compare(pos, 5, "begin") == 0 && isWordBoundary(text, pos, 5)) {
            ++depth;
            pos += 5;
            continue;
        }
        if (text.compare(pos, 3, "end") == 0 && isWordBoundary(text, pos, 3)) {
            --depth;
            pos += 3;
            if (depth == 0) {
                return pos;
            }
            continue;
        }
        ++pos;
    }
    throw std::runtime_error("Matching 'end' not found for 'begin'.");
}

void parseAlwaysBlocks(const std::string &text, EquationMap &equations) {
    size_t pos = 0;
    while (true) {
        pos = text.find("always", pos);
        if (pos == std::string::npos) {
            break;
        }
        if (!isAlwaysToken(text, pos)) {
            pos += 6;
            continue;
        }
        size_t beginPos = text.find("begin", pos);
        if (beginPos == std::string::npos) {
            break;
        }
        if (!isWordBoundary(text, beginPos, 5)) {
            pos = beginPos + 5;
            continue;
        }
        const auto endPos = findMatchingEnd(text, beginPos);
        const auto blockBody = text.substr(beginPos + 5, endPos - beginPos - 8); // exclude 'begin' and trailing 'end'
        BlockParser parser(blockBody);
        auto statements = parser.parseStatements();
        for (const auto &stmt : statements) {
            collectAssignments(*stmt, "", equations);
        }
        pos = endPos;
    }
}

class DependencySolver {
  public:
    DependencySolver(EquationMap equations, std::unordered_set<std::string> inputs, std::unordered_set<std::string> frozen)
        : equations_(std::move(equations)), inputSignals_(std::move(inputs)), frozenSymbols_(std::move(frozen)) {}

    std::string resolve(const std::string &symbol) {
        const auto trimmed = trim(symbol);
        if (trimmed.empty()) {
            return trimmed;
        }
        if (isBaseSymbol(trimmed)) {
            return trimmed;
        }
        if (auto it = cache_.find(trimmed); it != cache_.end()) {
            return it->second;
        }
        if (auto it = equations_.find(trimmed); it == equations_.end()) {
            cache_[trimmed] = trimmed;
            return trimmed;
        }
        if (recursionGuard_.count(trimmed)) {
            return trimmed;
        }
        recursionGuard_.insert(trimmed);
        std::string resolved = "1'bx";
        for (const auto &clause : equations_[trimmed]) {
            const auto guard = clause.condition.empty() ? "1'b1" : substituteIdentifiers(clause.condition);
            const auto value = substituteIdentifiers(clause.expression);
            resolved = "(" + guard + ") ? (" + value + ") : (" + resolved + ")";
        }
        recursionGuard_.erase(trimmed);
        cache_[trimmed] = resolved;
        return resolved;
    }

  private:
    EquationMap equations_;
    std::unordered_set<std::string> inputSignals_;
    std::unordered_set<std::string> frozenSymbols_;
    std::unordered_map<std::string, std::string> cache_;
    std::unordered_set<std::string> recursionGuard_;

    bool isNumber(const std::string &token) const {
        if (token.empty()) {
            return false;
        }
        if (std::isdigit(static_cast<unsigned char>(token.front()))) {
            return true;
        }
        if (token.size() > 2 && token[1] == '\'' ) {
            return true;
        }
        return false;
    }

    bool isBaseSymbol(const std::string &symbol) const {
        if (isNumber(symbol)) {
            return true;
        }
        if (inputSignals_.count(symbol)) {
            return true;
        }
        if (frozenSymbols_.count(symbol)) {
            return true;
        }
        return false;
    }

    std::string substituteIdentifiers(const std::string &expr) {
        static const std::regex identifier(R"(([A-Za-z_][A-Za-z0-9_$]*)(?:\.[A-Za-z_][A-Za-z0-9_$]*|\[[^\]]+\])*)");
        std::string output;
        size_t lastPos = 0;
        for (std::sregex_iterator iter(expr.begin(), expr.end(), identifier), end; iter != end; ++iter) {
            const auto matchPos = static_cast<size_t>(iter->position());
            output.append(expr.substr(lastPos, matchPos - lastPos));
            const auto token = iter->str();
            if (matchPos > 0 && expr[matchPos - 1] == '\'') {
                output.append(token);
            } else if (isBaseSymbol(token)) {
                output.append(token);
            } else {
                output.append("(" + resolve(token) + ")");
            }
            lastPos = matchPos + static_cast<size_t>(iter->length());
        }
        output.append(expr.substr(lastPos));
        return output;
    }
};

std::unordered_set<std::string> collectInputNames(const ModuleContext &ctx) {
    std::unordered_set<std::string> inputs;
    for (const auto &port : ctx.ports) {
        if (port.direction == "input" || port.direction == "inout") {
            inputs.insert(port.name);
        }
    }
    return inputs;
}

void writeEquations(const std::unordered_map<std::string, std::string> &resolved,
                    const std::filesystem::path &outputPath,
                    bool alsoStdout) {
    std::ofstream ofs(outputPath, std::ios::out | std::ios::trunc);
    if (!ofs) {
        throw std::runtime_error("Unable to write report file: " + outputPath.string());
    }
    for (const auto &pair : resolved) {
        const auto line = pair.first + " = " + pair.second;
        ofs << line << "\n\n";
        if (alsoStdout) {
            std::cout << line << "\n\n";
        }
    }
    ofs.close();
}

} // namespace

int main() {
    try {
        std::string verilogPathInput;
        std::cout << "Enter path to the Verilog/SystemVerilog file: ";
        std::getline(std::cin, verilogPathInput);
        verilogPathInput = trim(verilogPathInput);
        if (verilogPathInput.empty()) {
            throw std::runtime_error("Verilog file path is required.");
        }

        std::string moduleName;
        std::cout << "Enter the DUT module name: ";
        std::getline(std::cin, moduleName);
        moduleName = trim(moduleName);
        if (moduleName.empty()) {
            throw std::runtime_error("Module name is required.");
        }

        std::string frozenList;
        std::cout << "Enter comma-separated state/control signal names to keep symbolic (optional): ";
        std::getline(std::cin, frozenList);

        std::string outputPathInput;
        std::cout << "Enter output file path (leave blank for auto-named .txt): ";
        std::getline(std::cin, outputPathInput);

        const auto dutPath = std::filesystem::absolute(verilogPathInput);
        auto fileContent = readFile(dutPath);
        const auto cleaned = stripComments(fileContent);

        ModuleContext ctx;
        ctx.moduleName = moduleName;
        const auto portSection = extractPortRegion(cleaned, moduleName);
        ctx.ports = parsePorts(portSection);
        if (ctx.ports.empty()) {
            throw std::runtime_error("No ports parsed for module header. Ensure ANSI-style declarations.");
        }

        EquationMap equations;
        parseContinuousAssignments(cleaned, equations);
        parseAlwaysBlocks(cleaned, equations);

        std::unordered_set<std::string> frozenSymbols;
        std::stringstream fl(frozenList);
        std::string token;
        while (std::getline(fl, token, ',')) {
            const auto trimmedToken = trim(token);
            if (!trimmedToken.empty()) {
                frozenSymbols.insert(trimmedToken);
            }
        }

        auto inputSignals = collectInputNames(ctx);
        for (const auto &name : frozenSymbols) {
            inputSignals.erase(name); // ensure frozen signals stay separate
        }

        DependencySolver solver(equations, inputSignals, frozenSymbols);

        std::unordered_map<std::string, std::string> resolved;
        for (const auto &[signal, clauses] : equations) {
            if (inputSignals.count(signal)) {
                continue;
            }
            resolved[signal] = solver.resolve(signal);
        }

        const auto outputPath = outputPathInput.empty()
                                  ? std::filesystem::current_path() / defaultOutputPath(dutPath, moduleName)
                                  : std::filesystem::absolute(outputPathInput);

        std::cout << "\nDependency equations (also written to " << outputPath.string() << "):\n\n";
        writeEquations(resolved, outputPath, true);
    } catch (const std::exception &ex) {
        std::cerr << "Error: " << ex.what() << "\n";
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
