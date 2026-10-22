#pragma once

#include <algorithm>
#include <bit>
#include <bitset>
#include <cstdint>
#include <limits>
#include <map>
#include <numeric>
#include <set>
#include <sstream>
#include <string_view>
#include <utility>
#include <vector>

// ARJ decompression routine, based on code from https://github.com/EmbroidePy/pyembroidery/blob/main/pyembroidery/EmbCompress.py (MIT licensed)
// See https://github.com/software-opal/archivelib-rs/issues/3 for some compression algorithm details

class Huffman {
private:
    std::uint16_t default_value;
	  std::vector<std::uint8_t> lengths; // Up to 16
	  std::vector<std::pair<std::bitset<16>, std::uint16_t> > canonical_codes; // Map canonical huffman codes to index - O(N) lookup, smaller table
public:
	  Huffman(std::uint16_t value = 0) : default_value(value) {}
	  Huffman(std::vector<std::uint8_t> lengths, std::uint16_t value = 0) : default_value(value), lengths(lengths) {
		std::vector<std::size_t> sorted_length_indices(lengths.size());
#ifdef  __cpp_lib_ranges_iota
		std::ranges::iota(sorted_length_indices, 0);
#else
		std::iota(sorted_length_indices.begin(), sorted_length_indices.end(), 0);
#endif

		// Find the sorted indices for each length position, stable sort is required to keep ties in the original order
		std::ranges::stable_sort(sorted_length_indices, [&lengths](auto i1, auto i2) -> bool {
			return (lengths[i1] < lengths[i2]);
		});

		std::uint16_t pos = 0;
		for (std::size_t i = 0; i < sorted_length_indices.size(); ++i) {
			auto currlength_index = sorted_length_indices[i];
			auto nextlength_index = (i == sorted_length_indices.size() - 1) ? currlength_index : sorted_length_indices[i + 1];
			std::size_t currlength = lengths[currlength_index];
			if (currlength != 0) {
				std::size_t nextlength = lengths[nextlength_index];
				canonical_codes.emplace_back(std::bitset<16>(pos), static_cast<std::uint16_t>(currlength_index));
				++pos;
				pos <<= nextlength - currlength;
			}
		}
	}

	std::pair<std::uint16_t, std::uint8_t> lookup(std::bitset<16> byte_lookup) {
		for (const auto &[key, length_idx] : canonical_codes) {
			if ((byte_lookup >> (16 - lengths[length_idx])) == key) {
				return { length_idx, lengths[length_idx] };
			}
		}
		return { default_value, std::int8_t(0) };
	}
};

class BitArray {
public:
	// Construct bit array from input character data
	BitArray(const std::vector<unsigned char>& input) : data(input) {
		bits_total = data.size() << 3; // Multiply by 8
	}

	// Return next N bits and increment current position by N
	template<std::size_t N>
	std::bitset<N> pop() {
		auto value = peek<N>();
		slide(N);
		return value;
	}

	// Return next N bits
	template<std::size_t N>
	std::bitset<N> peek() {
		return get<N>();
	}

	// Increment current position in array
	void slide(std::size_t bit_count) {
		bit_position += bit_count;
	}

	// Any data left to be read?
	bool empty() const {
		return !(bit_position < bits_total);
	}

private:
	std::size_t bits_total = 0;
	std::size_t bit_position = 0;
	const std::vector<unsigned char>& data;

	// Return N bits from current position
	template<std::size_t N>
	std::bitset<N> get() {
		std::bitset<N> result;
		for (std::size_t i = bit_position; i < bit_position + N; ++i) {
			std::size_t byte_position = i >> 3;
			if (byte_position >= data.size()) {
				break;
			}
			std::bitset<8> val(data[byte_position]);
			auto in_idx = (i & 0x7);
			if constexpr (std::endian::native == std::endian::little) {
				in_idx = 7 - in_idx;
			}
			auto out_idx = (i - bit_position);
			if constexpr (std::endian::native == std::endian::little) {
				out_idx = (N - 1) - out_idx;
			}
			result.set(out_idx, val[in_idx]);
		}

		return result;
	}
};

class ARJDecompress {
public:
	ARJDecompress(const std::vector<unsigned char>& input) : input_data(BitArray(input)), output_data(std::stringstream::binary | std::stringstream::in | std::stringstream::out) {
		while (input_data.empty() == false) {
			auto character = get_token();
			if (character <= 255) {
				output_data.write((char*)&character, 1);
			}
			else if (character == 510) {
				break;
			}
			else {
				std::uint16_t length = character - 253;
				std::size_t back = get_position() + 1;
				output_data.seekg(-static_cast<std::int64_t>(back), std::ios_base::end);
				// Note: Can't read/write in larger blocks, as some input data may only be written within the loop
				// We could check this with back > length check, but this would also require allocating a buffer
				for (std::uint16_t i = 0; i < length; ++i) {
					unsigned char c = 0;
					output_data.read((char*)&c, 1);
					output_data.write((char*)&c, 1);
				}
			}
		}
		output_data.seekg(0);
	}

	std::stringstream& data() {
		return output_data;
	}

private:
	std::int32_t block_elements = -1;
	Huffman character_huffman;
	Huffman distance_huffman;
	BitArray input_data;
	std::stringstream output_data;
	
	std::uint8_t read_variable_length() {
		auto m = static_cast<std::uint16_t>(input_data.peek<16>().to_ulong());
		std::uint8_t firstthree = m >> 13;
		if (firstthree != 7) {
			input_data.slide(3);
			return firstthree;
		}
		else {
			std::uint8_t count = static_cast<std::uint8_t>(std::countl_one(m));
			input_data.slide(count + 1);
			return count + 4; // 7 + number of "1" bits after first three
		}
	}

	Huffman load_character_length_huffman() {
		std::uint8_t count = static_cast<std::uint8_t>(input_data.pop<5>().to_ulong()); // Maximum value: 31
		if (count == 0) {
			auto v = static_cast<std::uint16_t>(input_data.pop<5>().to_ulong());
			return Huffman(v);
		}
		else {
			std::vector<std::uint8_t> huffman_code_lengths(count, 0);
			for (std::uint8_t index = 0; index < count; ++index) {
				if (index == 3) { // Special index 3, skip up to 3 elements.
					index += static_cast<std::uint8_t>(input_data.pop<2>().to_ulong());
				}
				huffman_code_lengths[index] = read_variable_length();
			}
			return Huffman(huffman_code_lengths, 8);
		}
	}

	Huffman load_character_huffman(Huffman length_huffman) {
		auto count = input_data.pop<9>().to_ullong();
		if (count == 0) {
			auto v = static_cast<std::uint16_t>(input_data.pop<9>().to_ulong());
			return Huffman(v);
		}
		else {
			std::vector<std::uint8_t> huffman_code_lengths(count, 0);
			for (std::size_t index = 0; index < count; ++index) {
				auto [c, length] = length_huffman.lookup(input_data.peek<16>());
				input_data.slide(length);
				if (c == 0) {
					// C == 0, skip 1.
				}
				else if (c == 1) { 
					// C == 1, skip 3 + read(4)
					index += 2 + static_cast<std::uint16_t>(input_data.pop<4>().to_ulong());
				}
				else if (c == 2) {
					// C == 2, skip 20 + read(9)
					index += 19 + static_cast<std::uint16_t>(input_data.pop<9>().to_ulong());
				}
				else {
					huffman_code_lengths[index] = static_cast<std::uint8_t>(c - 2);
				}
			}
			return Huffman(huffman_code_lengths);
		}
	}

	Huffman load_distance_huffman() {
		auto count = input_data.pop<5>().to_ulong();
		if (count == 0) {
			auto v = static_cast<std::uint16_t>(input_data.pop<5>().to_ulong());
			return Huffman(v);
		}
		else {
			std::vector<std::uint8_t> lengths(count, 0);
			for (std::uint8_t index = 0; index < count; ++index) {
				lengths[index] = read_variable_length();
			}
			return Huffman(lengths);
		}
	}

	void load_block() {
		block_elements = input_data.pop<16>().to_ulong();
		Huffman character_length_huffman = load_character_length_huffman();
		character_huffman = load_character_huffman(character_length_huffman);
		distance_huffman = load_distance_huffman();
	}

	std::uint16_t get_token() {
		if (block_elements <= 0) {
			load_block();
		}
		block_elements -= 1;
		auto [c, length] = character_huffman.lookup(input_data.peek<16>());
		input_data.slide(length);
		return c;
	}

	std::size_t get_position() {
		auto [c, length] = distance_huffman.lookup(input_data.peek<16>());
		input_data.slide(length);
		if (c == 0) {
			return 0;
		}
		std::size_t v = static_cast<std::uint16_t>(c - 1);

		std::size_t offset = 0;
		// Maximum 64, as this is used as a bit position (probably really limited to 16)
		switch (v) {
		case 0:
			break;
		case 1:
			offset = input_data.pop<1>().to_ullong();
			break;
		case 2:
			offset = input_data.pop<2>().to_ullong();
			break;
		case 3:
			offset = input_data.pop<3>().to_ullong();
			break;
		case 4:
			offset = input_data.pop<4>().to_ullong();
			break;
		case 5:
			offset = input_data.pop<5>().to_ullong();
			break;
		case 6:
			offset = input_data.pop<6>().to_ullong();
			break;
		case 7:
			offset = input_data.pop<7>().to_ullong();
			break;
		case 8:
			offset = input_data.pop<8>().to_ullong();
			break;
		case 9:
			offset = input_data.pop<9>().to_ullong();
			break;
		case 10:
			offset = input_data.pop<10>().to_ullong();
			break;
		case 11:
			offset = input_data.pop<11>().to_ullong();
			break;
		case 12:
			offset = input_data.pop<12>().to_ullong();
			break;
		case 13:
			offset = input_data.pop<13>().to_ullong();
			break;
		case 14:
			offset = input_data.pop<14>().to_ullong();
			break;
		case 15:
			offset = input_data.pop<15>().to_ullong();
			break;
		case 16:
			offset = input_data.pop<16>().to_ullong();
			break;
		case 17:
			offset = input_data.pop<17>().to_ullong();
			break;
		case 18:
			offset = input_data.pop<18>().to_ullong();
			break;
		case 19:
			offset = input_data.pop<19>().to_ullong();
			break;
		case 20:
			offset = input_data.pop<20>().to_ullong();
			break;
		case 21:
			offset = input_data.pop<21>().to_ullong();
			break;
		case 22:
			offset = input_data.pop<22>().to_ullong();
			break;
		case 23:
			offset = input_data.pop<23>().to_ullong();
			break;
		case 24:
			offset = input_data.pop<24>().to_ullong();
			break;
		case 25:
			offset = input_data.pop<25>().to_ullong();
			break;
		case 26:
			offset = input_data.pop<26>().to_ullong();
			break;
		case 27:
			offset = input_data.pop<27>().to_ullong();
			break;
		case 28:
			offset = input_data.pop<28>().to_ullong();
			break;
		case 29:
			offset = input_data.pop<29>().to_ullong();
			break;
		case 30:
			offset = input_data.pop<30>().to_ullong();
			break;
		case 31:
			offset = input_data.pop<31>().to_ullong();
			break;
		case 32:
			offset = input_data.pop<32>().to_ullong();
			break;
		case 33:
			offset = input_data.pop<33>().to_ullong();
			break;
		case 34:
			offset = input_data.pop<34>().to_ullong();
			break;
		case 35:
			offset = input_data.pop<35>().to_ullong();
			break;
		case 36:
			offset = input_data.pop<36>().to_ullong();
			break;
		case 37:
			offset = input_data.pop<37>().to_ullong();
			break;
		case 38:
			offset = input_data.pop<38>().to_ullong();
			break;
		case 39:
			offset = input_data.pop<39>().to_ullong();
			break;
		case 40:
			offset = input_data.pop<40>().to_ullong();
			break;
		case 41:
			offset = input_data.pop<41>().to_ullong();
			break;
		case 42:
			offset = input_data.pop<42>().to_ullong();
			break;
		case 43:
			offset = input_data.pop<43>().to_ullong();
			break;
		case 44:
			offset = input_data.pop<44>().to_ullong();
			break;
		case 45:
			offset = input_data.pop<45>().to_ullong();
			break;
		case 46:
			offset = input_data.pop<46>().to_ullong();
			break;
		case 47:
			offset = input_data.pop<47>().to_ullong();
			break;
		case 48:
			offset = input_data.pop<48>().to_ullong();
			break;
		case 49:
			offset = input_data.pop<49>().to_ullong();
			break;
		case 50:
			offset = input_data.pop<50>().to_ullong();
			break;
		case 51:
			offset = input_data.pop<51>().to_ullong();
			break;
		case 52:
			offset = input_data.pop<52>().to_ullong();
			break;
		case 53:
			offset = input_data.pop<53>().to_ullong();
			break;
		case 54:
			offset = input_data.pop<54>().to_ullong();
			break;
		case 55:
			offset = input_data.pop<55>().to_ullong();
			break;
		case 56:
			offset = input_data.pop<56>().to_ullong();
			break;
		case 57:
			offset = input_data.pop<57>().to_ullong();
			break;
		case 58:
			offset = input_data.pop<58>().to_ullong();
			break;
		case 59:
			offset = input_data.pop<59>().to_ullong();
			break;
		case 60:
			offset = input_data.pop<60>().to_ullong();
			break;
		case 61:
			offset = input_data.pop<61>().to_ullong();
			break;
		case 62:
			offset = input_data.pop<62>().to_ullong();
			break;
		case 63:
			offset = input_data.pop<63>().to_ullong();
			break;
		case 64:
			offset = input_data.pop<64>().to_ullong();
			break; 
		default:
			// Invalid input
			return 0;
		}
		
		v = (std::size_t(1) << v) + offset;
		return v;
	}
};