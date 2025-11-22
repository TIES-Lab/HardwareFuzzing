#include <iostream>
#include <fstream>
#include <vector>
#include <regex>


// This struct contains relevant information regarding a register. (i.e., name, bitwidth etc.)
struct reg_det{
    std::string name;
    std::string arr_value;
    int bitwidth;
};

// A class containing all the module information. This class will be used to mutate the seeds.
class module_info{
private:
    //  Rrgister detail containers
    std::vector<reg_det> module_input, module_output;


    std::vector <reg_det> reg_name_separation(std::string line, std::string delimiter, std::regex pattern){
        std::vector<reg_det> out_list;
        size_t pos = 0;
        reg_det token;
        std::string reg_names, arr_size;

        // Array pattern to calculate the size of a register in a design;
        std::regex arr_pattern{"\\[\\s*([\\w\\d]+)\\s*:\\s*([\\w\\d]+)\\s*\\]"};
        std::smatch match, len_match;

        if(std::regex_search(line, match, pattern)){            // If the pattern is found in the line this block is executed
            arr_size = match[2];
            token.arr_value = arr_size;
            if (arr_size.length() == 0) token.bitwidth = 1;
            else {
                std::regex_search(arr_size, len_match, arr_pattern);
                token.bitwidth = std::abs(std::stoi(len_match[2]) - std::stoi(len_match[1])) + 1;
            }
            reg_names = std::regex_replace(match[3].str(), std::regex("^\\s+|\\s+$"), "");
            if(reg_names.find(delimiter) == std::string::npos){
                token.name = reg_names;
                out_list.push_back(token);
            } else {
                while((pos = reg_names.find(delimiter)) != std::string::npos){
                    token.name = reg_names.substr(0, pos);
                    token.name = token.name;
                    reg_names.erase(0, pos+delimiter.length());
                    out_list.push_back(token);
                }
            }
        }
        return out_list;
    }

public:
    void parse_file(std::string fileName){
        /*
        This function parses a Verilog/SystemVerilog file and extracts the module name, input and output pins.
        */

        if (fileName.empty()) {
            throw std::invalid_argument("Filename cannot be empty");
        }
    
        if (fileName.substr(fileName.find_last_of(".") + 1) != "v" && fileName.substr(fileName.find_last_of(".") + 1) != "sv") {
            throw std::invalid_argument("Error: File must have a .v or .sv extension");
        }
    
        std::ifstream infile(fileName);
        if (!infile.is_open()) {
            throw std::runtime_error("Error: Could not open file " + fileName);
        }

        std::stringstream noCommentFile = remove_comments(fileName);

        std::string line;    
        while (std::getline(noCommentFile, line)){
            get_inouts(line);
        }
    }

    std::stringstream remove_comments(std::string fileName){
        /*
        This function parses a Verilog/SystemVerilog file, removes the comments and empty lines
        and returns the processed content as a stringstream.
        */

        std::string line;
        std::ifstream inFile(fileName);
        std::stringstream outFile;

        std::regex inline_Comment_pattern{"\\s*//.*"};          // Inline comment pattern
        std::regex block_Comment_strt_pattern{"/\\*.*"};        // Block comment start pattern 
        std::regex block_Comment_end_pattern{".*\\*/"};         // Block comment end pattern
        std::regex blank_line_pattern{"\\s*"};                  // Empty Line pattern
        std::smatch match, mtch;

        bool commentStat = 0;                                   // Flag to determine if it is a comment or not

        while (std::getline(inFile, line)){
            if (std::regex_search(line, match, block_Comment_strt_pattern)){
                commentStat = 1;
                line = std::regex_replace(line, block_Comment_strt_pattern, "");
            } else if (std::regex_search(line, match, block_Comment_end_pattern)){
                line = std::regex_replace(line, block_Comment_end_pattern, "");
                commentStat = 0;
            }
            // Writing into the output stringstream without any comments
            if (!commentStat){      // If it is not a comment write into the output, otherwise ignore.
                if (std::regex_search(line, match, inline_Comment_pattern)){
                    line = std::regex_replace(line, inline_Comment_pattern, "");   
                }
            } else line = "";       // If a line is comment replace it with a blank string
            if (!std::regex_match(line, mtch, blank_line_pattern)) outFile << line << std::endl;    // If line number is not empty, write it to the new file
        }
        return outFile;
    }


    void get_inouts(std::string line){
        /*
         * This function will read each line and find input and outputs of the module.
         */
        

        // Pattern for input declaration
        std::regex input_pattern{"\\s*input\\s*(reg|logic|bit\\s*)?(\\[[:\\d\\w]+\\])?([\\s\\w\\d,{}]*)\\s*"};
        // Pattern for output declaration
        std::regex output_pattern{"\\s*output\\s*(reg|logic|bit\\s*)?(\\[[:\\d\\w]+\\])?([\\s\\w\\d,{}]*)\\s*"};

        std::string delimiter = ",";    // Delimiter string

        std::smatch mtch;
        std::vector<reg_det> temp;

        if(std::regex_search(line, mtch, input_pattern)){
            temp = reg_name_separation(line, delimiter, input_pattern);
            module_input.insert(module_input.end(), temp.begin(), temp.end());
        } else if(std::regex_search(line, mtch, output_pattern)){
            temp = reg_name_separation(line, delimiter, output_pattern);
            module_output.insert(module_output.end(), temp.begin(), temp.end());
        }
    }

    void show_inouts(){
        std::cout << "Input Pins:" << std::endl;
        for (reg_det x : module_input){
            std::cout <<x.name << "\t" << x.arr_value << "\t" << x.bitwidth << std::endl;
        }
        std::cout << "Output Pins:" << std::endl;
        for (reg_det x : module_output){
            std::cout << x.name << "\t" << x.arr_value << "\t" << x.bitwidth << std::endl;
        }
    }
};

void remove_comments(std::string fileName, std::string outFileName){
    /*
    This function parses a Verilog/SystemVerilog file, removes the comments and empty lines
    it takes the input and output file names. It does not have a return value.
    */

    std::string line;

    std::ifstream inFile(fileName);
    std::ofstream outFile;


    std::regex inline_Comment_pattern{"\\s*//.*"};          // Inline comment pattern
    std::regex block_Comment_strt_pattern{"/\\*.*"};        // Block comment start pattern 
    std::regex block_Comment_end_pattern{".*\\*/"};         // Block comment end pattern
    std::regex blank_line_pattern{"\\s*"};                  // Empty Line pattern
    std::smatch match, mtch;

    bool commentStat = 0;                                   // Flag to determine if it is a comment or not

    outFile.open(outFileName);
    while (std::getline(inFile, line)){
        if (std::regex_search(line, match, block_Comment_strt_pattern)){
            commentStat = 1;
            line = std::regex_replace(line, block_Comment_strt_pattern, "");
        } else if (std::regex_search(line, match, block_Comment_end_pattern)){
            line = std::regex_replace(line, block_Comment_end_pattern, "");
            commentStat = 0;
        }
        // Writing into the output file without any comments
        if (!commentStat){      // If it is not a comment write into the output, otherwise ignore.
            if (std::regex_search(line, match, inline_Comment_pattern)){
                line = std::regex_replace(line, inline_Comment_pattern, "");   
            }
        } else line = "";       // If a line is comment replace it with a blank string
        if (!std::regex_match(line, mtch, blank_line_pattern)) outFile << line << std::endl;    // If line number is not empty, write it to the new file
    }
    outFile.close();
}

int main(int argc, char** argv) {

    module_info get_io;
    get_io.parse_file(argv[1]);
    get_io.show_inouts();

    return 0;
}
