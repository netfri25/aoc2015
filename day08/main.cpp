#include <iostream>
#include <fstream>
#include <vector>
#include <string>

std::vector<std::string> read_lines(std::string const path) {
    std::ifstream file(path);
    std::vector<std::string> output;

    std::string line;
    while (line.clear(), std::getline(file, line)) {
        output.push_back(line);
    }

    return output;
}

std::string decode(std::string_view const input) {
    std::string_view const inside = input.substr(1, input.length() - 2);
    std::string output;

    for (auto iter = inside.cbegin(); iter != inside.cend(); iter++) {
        if (*iter != '\\') {
            output.push_back(*iter);
            continue;
        }

        iter++; // consume the backslash
        switch (*iter) {
        break; case '\\': output.push_back('\\');
        break; case '\"': output.push_back('\"');
        break; case 'x': {
            char digits[3] = {0};
            digits[0] = *++iter;
            digits[1] = *++iter;
            unsigned char byte = strtoul(digits, nullptr, 16);
            output.push_back(byte);
        };
        }
    }

    return output;
}

std::string encode(std::string_view input) {
    std::string output;

    output.push_back('"');
    for (char c : input) {
        if (c == '"' || c == '\\') {
            output.push_back('\\');
        }

        output.push_back(c);
    }
    output.push_back('"');

    return output;
}

auto main() -> int {
    std::vector<std::string> literal = read_lines("./input.txt");

    // part 1
    {
        std::vector<std::string> decoded;
        for (auto const& line : literal) {
            decoded.push_back(decode(line));
        }

        size_t total = 0;
        for (size_t i = 0; i < literal.size(); i++) {
            total += literal[i].length() - decoded[i].length();
        }

        std::cout << "part 1: " << total << std::endl;
    }

    // part 2
    {
        std::vector<std::string> encoded;
        for (auto const& line : literal) {
            encoded.push_back(encode(line));
        }

        size_t total = 0;
        for (size_t i = 0; i < literal.size(); i++) {
            total += encoded[i].length() - literal[i].length();
        }

        std::cout << "part 2: " << total << std::endl;
    }
}
