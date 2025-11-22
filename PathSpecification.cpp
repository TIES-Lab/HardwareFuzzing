#include <iostream>
#include <regex>
#include <string>
#include <vector>
#include <fstream>
#include <stdexcept>

// Struct to represent a CFG point detail
struct CFGPointDetail {
    std::string name;
    std::string node;
    std::string statement;
    std::string type;
};

// Function to separate the string based on "::" and return a CFGPointDetail struct
CFGPointDetail separateString(const std::string& input) {
    CFGPointDetail cfgPointDetail;
    std::regex delimiter("::");
    std::sregex_token_iterator iter(input.begin(), input.end(), delimiter, -1);
    std::sregex_token_iterator end;

    std::vector<std::string> parts;
    for (; iter != end; ++iter) {
        parts.push_back(iter->str());
    }

    if (parts.size() >= 4) {
        cfgPointDetail.name = std::regex_replace(parts[0], std::regex("^\\s+|\\s+$"), "");
        cfgPointDetail.node = std::regex_replace(parts[1], std::regex("^\\s+|\\s+$"), "");
        cfgPointDetail.statement = std::regex_replace(parts[2], std::regex("^\\s+|\\s+$"), "");
        cfgPointDetail.type = std::regex_replace(parts[3], std::regex("^\\s+|\\s+$|;"), "");
    } else {
        throw std::invalid_argument("Input string does not contain enough parts.");
    }

    return cfgPointDetail;
}

// Function to parse a file and return its content as a string
std::string parseFile(const std::string& filePath) {
    std::ifstream file(filePath);
    if (!file.is_open()) {
        throw std::runtime_error("Unable to open file: " + filePath);
    }

    std::string content((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    file.close();
    return content;
}

// Function to parse and process a file into a vector of CFGPointDetail structs
std::vector<CFGPointDetail> parseAndProcessFile(const std::string& filePath) {
    std::ifstream file(filePath);
    if (!file.is_open()) {
        throw std::runtime_error("Unable to open file: " + filePath);
    }

    std::vector<CFGPointDetail> cfgDetails;
    std::string line;

    while (std::getline(file, line)) {
        try {
            CFGPointDetail detail = separateString(line);
            cfgDetails.push_back(detail);
        } catch (const std::invalid_argument& e) {
            std::cerr << "Skipping line due to error: " << e.what() << " Line: " << line << std::endl;
        }
    }

    file.close();
    return cfgDetails;
}

// Function to find the next struct based on the given criteria
CFGPointDetail findNextStruct(const CFGPointDetail& current, const std::vector<CFGPointDetail>& cfgDetails) {
    if (current.type == "A") {
        // If the type is "A", find the next struct with the same node but type "C"
        for (const auto& detail : cfgDetails) {
            if (detail.node == current.node && detail.type == "C") {
                return detail;
            }
        }
    } else if (current.type == "C") {
        // If the type is "C", reduce the node by removing the last item (comma-separated)
        size_t lastComma = current.node.find_last_of(',');
        std::string reducedNode = (lastComma != std::string::npos) ? current.node.substr(0, lastComma) : "";

        // Find the next struct with the reduced node
        for (const auto& detail : cfgDetails) {
            if (detail.node == reducedNode) {
                return detail;
            }
        }
    }

    // If no matching struct is found, throw an exception
    throw std::runtime_error("Next struct not found based on the given criteria.");
}

// Function to construct a sequence of CFGPointDetail structs until a type "Alws" is found or no more structs exist
std::vector<CFGPointDetail> constructSequenceUntilAlws(const CFGPointDetail& start, const std::vector<CFGPointDetail>& cfgDetails) {
    std::vector<CFGPointDetail> sequence;
    CFGPointDetail current = start;

    try {
        while (true) {
            sequence.push_back(current);

            // If the current struct is of type "Alws", stop and add it to the sequence
            if (current.type == "Alws") {
                break;
            }

            // Find the next struct
            current = findNextStruct(current, cfgDetails);
        }
    } catch (const std::runtime_error& e) {
        // No more next struct, exit the loop
    }

    return sequence;
}

// Main function
int main() {
    try {
        // Example input string
        std::string input = "Module: memory_control :: 1, 3, 0 :: ccif.dwait[~dsource] <== 0 :: A;";
        CFGPointDetail parts = separateString(input);

        // File path to the memory control file
        std::string inputPath = "D:\\DAC_Ext\\Extracted_CFG\\memory_control.txt";

        // Parse the file content
        std::string fileContent = parseFile(inputPath);
        std::cout << "File content:\n" << fileContent << "\n";

        // Parse and process the file into CFGPointDetail structs
        std::vector<CFGPointDetail> cfgDetails = parseAndProcessFile(inputPath);

        // Display parsed CFG details
        std::cout << "\nParsed CFG Details:\n";
        for (const auto& detail : cfgDetails) {
            std::cout << "Name: " << detail.name << ", Node: " << detail.node
                      << ", Statement: " << detail.statement << ", Type: " << detail.type << "\n";
        }

        // Find the next struct
        std::cout << "\nFinding next struct...\n";
        CFGPointDetail nextStruct = findNextStruct(parts, cfgDetails);
        std::cout << "Next Struct - Name: " << nextStruct.name << ", Node: " << nextStruct.node
                  << ", Statement: " << nextStruct.statement << ", Type: " << nextStruct.type << "\n";

        // Construct the sequence until "Alws"
        std::cout << "\nConstructing sequence until Alws...\n";
        std::vector<CFGPointDetail> sequence = constructSequenceUntilAlws(parts, cfgDetails);

        // Display the constructed sequence
        std::cout << "Constructed Sequence:\n";
        for (const auto& detail : sequence) {
            std::cout << "Name: " << detail.name << ", Node: " << detail.node
                      << ", Statement: " << detail.statement << ", Type: " << detail.type << "\n";
        }
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
    }

    return 0;
}
