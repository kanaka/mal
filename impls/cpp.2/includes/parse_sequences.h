#ifndef PARSE_SEQUENCES_H
#define PARSE_SEQUENCES_H

void read_list(std::string input_stream, TokenVector& tokens);
void close_list();
void read_vector(std::string input_stream, TokenVector& tokens);
void close_vector();
void read_hashmap(std::string input_stream, TokenVector& tokens);
void close_hashmap();

#endif