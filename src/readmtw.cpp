// [[Rcpp::plugins(cpp23)]]

#include <Rcpp.h>

#include "arj.h"
#include "pole.h"
#include "utf8.h"

#include <bit>
#include <chrono>
#include <codecvt>
#include <format>
#include <fstream>
#include <iostream>
#ifdef __cpp_lib_stacktrace
#include <stacktrace>
#endif
#include <string>
#include <string_view>
#include <unordered_map>
#include <version>

[[nodiscard]] static std::string_view trim(std::string_view str) {
  str.remove_prefix(std::min(str.find_first_not_of(" "), str.size()));
  auto end_pos = str.find_last_not_of(" ");
  if (end_pos != std::string_view::npos) {
    str.remove_suffix(str.size() - end_pos - 1);
  }
  return str;
}

[[nodiscard]] static bool readRec(POLE::Stream& str, void* data, std::size_t size) {
  POLE::uint64 BytesRead = str.read((unsigned char*)data, size);
  if (size != BytesRead) {
    Rcpp::warning("Read error: expected %i bytes, got %i", size, BytesRead);
#ifdef __cpp_lib_stacktrace
    Rcpp::Rcout << std::stacktrace::current() << '\n'
#endif
    return false;
  }
  else {
    return true;
  }
}

template <typename T>
[[nodiscard]] static T readValue(std::istream& str) {
  T result{};
  str.read((char*)&result, sizeof(T));
  if constexpr (std::endian::native != std::endian::little) {
#ifdef __cpp_lib_byteswap
    // NOTE: This is untested as I don't have a big endian machine
    if constexpr (std::is_same_v<T, float>) {
      result = std::bit_cast<float>(std::byteswap(std::bit_cast<std::uint32_t>(result)));
    } 
    else if constexpr (std::is_same_v<T, double>) {
      result = std::bit_cast<double>(std::byteswap(std::bit_cast<std::uint64_t>(result)));
    } 
    else {
      result = std::byteswap(result);
    }
#endif
  }
  return result;
}

template <typename T>
[[nodiscard]] static T readValue(POLE::Stream& str) {
  T result{};
  if (readRec(str, &result, sizeof(T)) == false) return result;
  if constexpr (std::endian::native != std::endian::little) {
#ifdef __cpp_lib_byteswap
    // NOTE: This is untested as I don't have a big endian machine
    if constexpr (std::is_same_v<T, float>) {
      result = std::bit_cast<float>(std::byteswap(std::bit_cast<std::uint32_t>(result)));
    } 
    else if constexpr (std::is_same_v<T, double>) {
      result = std::bit_cast<double>(std::byteswap(std::bit_cast<std::uint64_t>(result)));
    } 
    else {
      result = std::byteswap(result);
    }
#endif
  }
  return result;
}

// Note:
// Replaced non-breaking space (00A0) with space (0020)
// Replaced soft-hyphen (00AD) with hyphen (002D)
static constexpr std::string_view extchars[] {
  "\u20AC", "\u0020", "\u201A", "\u0192", "\u201E", "\u2026", "\u2020", "\u2021",
  "\u02C6", "\u2030", "\u0160", "\u2039", "\u0152", "\u0020", "\u017D", "\u0020",
  "\u0020", "\u2018", "\u2019", "\u201C", "\u201D", "\u2022", "\u2013", "\u2014",
  "\u02DC", "\u2122", "\u0161", "\u203A", "\u0153", "\u0020", "\u017E", "\u0178",
  "\u0020", "\u00A1", "\u00A2", "\u00A3", "\u00A4", "\u00A5", "\u00A6", "\u00A7",
  "\u00A8", "\u00A9", "\u00AA", "\u00AB", "\u00AC", "\u0045", "\u00AE", "\u00AF",
  "\u00B0", "\u00B1", "\u00B2", "\u00B3", "\u00B4", "\u00B5", "\u00B6", "\u00B7",
  "\u00B8", "\u00B9", "\u00BA", "\u00BB", "\u00BC", "\u00BD", "\u00BE", "\u00BF",
  "\u00C0", "\u00C1", "\u00C2", "\u00C3", "\u00C4", "\u00C5", "\u00C6", "\u00C7",
  "\u00C8", "\u00C9", "\u00CA", "\u00CB", "\u00CC", "\u00CD", "\u00CE", "\u00CF",
  "\u00D0", "\u00D1", "\u00D2", "\u00D3", "\u00D4", "\u00D5", "\u00D6", "\u00D7",
  "\u00D8"," \u00D9", "\u00DA", "\u00DB", "\u00DC", "\u00DD", "\u00DE", "\u00DF",
  "\u00E0"," \u00E1", "\u00E2", "\u00E3", "\u00E4", "\u00E5", "\u00E6", "\u00E7",
  "\u00E8", "\u00E9", "\u00EA", "\u00EB", "\u00EC", "\u00ED", "\u00EE", "\u00EF",
  "\u00F0", "\u00F1", "\u00F2", "\u00F3", "\u00F4", "\u00F5", "\u00F6", "\u00F7",
  "\u00F8", "\u00F9", "\u00FA", "\u00FB", "\u00FC", "\u00FD", "\u00FE", "\u00FF"
};

// Convert extended ANSI codes to UTF-8 if required
[[nodiscard]] static std::string ANSI2UTF8(const std::string &input) {
  std::string result{};
  for (std::uint8_t c : input) {
    if (c < 128) {
      result += c;
    }
    else {
      result += extchars[c - 128];
    }
  }
  return result;
}

[[nodiscard]] static std::string readString(std::istream& str, std::size_t len) {
  std::string result(len, ' ');
  str.read((char*)result.data(), result.size());
  auto nullpos = result.find('\0');
  if (nullpos != std::string::npos) {
    result.erase(nullpos);
  }
  return ANSI2UTF8(result);
}

[[nodiscard]] static std::string readString(POLE::Stream& str, std::size_t len) {
  std::string result(len, ' ');
  if (readRec(str, result.data(), result.length()) == false) return result;
  auto nullpos = result.find('\0');
  if (nullpos != std::string::npos) {
    result.erase(nullpos);
  }
  return ANSI2UTF8(result);
}

[[nodiscard]] static std::string readString(POLE::Stream& str) {
  std::string result{};
  std::uint32_t size = readValue<std::uint32_t>(str);
  if (size > 0) {
    result = readString(str, size);
  }
  return result;
}

static void skipBytes(std::istream& str, std::size_t off) {
  str.seekg(off, std::ios::cur);
}

static void skipBytes(POLE::Stream& str, std::size_t off) {
  auto pos = str.tell();
  pos += off;
  str.seek(pos);
}

[[nodiscard]] static std::string readWString(POLE::Stream& str) {
  std::string result{};
  std::uint64_t size = readValue<std::uint64_t>(str);
  std::uint32_t version = readValue<std::uint32_t>(str); // 1
  if (version != 1) {
    Rcpp::warning("Unexpected version for string record: %i", version);
  }
  std::uint32_t slen = readValue<std::uint32_t>(str);
  if (slen * 2 != size - 16) {
    Rcpp::warning("Unexpected record size reading string %i expected %i", size, (2 * slen) + 16);
    slen = static_cast<std::uint32_t>((size - 16) / 2);
  }
  std::u16string s(slen, ' '); // 2-byte characters
  if (readRec(str, s.data(), sizeof(char16_t) * s.length()) == false) return result;
  
  // Replaced following line a with call to utfcpp library to pass CRAN checks, as it causes 
  // a deprecation warning
  // result = std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t>{}.to_bytes(s);
  result = utf8::utf16to8(s);
  
  auto nullpos = result.find('\0');
  if (nullpos != std::string::npos) {
    result.erase(nullpos);
  }
  return result;
}

[[nodiscard]] static std::tuple<std::uint32_t, std::uint32_t, std::string> readAddr(POLE::Stream& str) {
  std::string addrstr = readString(str, 4); // ADDR
  if (addrstr != "ADDR") {
    Rcpp::warning("Unexpected string: expected ADDR, got %s", addrstr);
  }
  std::uint32_t addr = readValue<std::uint32_t>(str);
  // e.g. Column: 434, Constant: 435, Matrix: 436
  std::uint32_t objclass = readValue<std::uint32_t>(str); 
  // e.g. Column: CmColumnVar, Constant: CmConstantVar, Matrix: CmMatrixVar
  std::string id = (objclass != 0) ? readString(str) : ""; 
  id = trim(id);
  return { addr, objclass, id };
}

template <typename recheadT, typename floatT>
static Rcpp::List readOld(std::iostream &fs, bool haslabels) {
  Rcpp::List constants;
  Rcpp::List columns;
  Rcpp::List matrices;
  std::string author{};
  std::string date{};
  std::string description{};
  while (fs.good()) {
    recheadT rectype = readValue<recheadT>(fs);
    recheadT objid = readValue<recheadT>(fs);
    recheadT numrec = readValue<recheadT>(fs);
    recheadT recsize = readValue<recheadT>(fs);
    // record size rounded up to 4 byte boundary
    recheadT blocksize = ((-recsize + 3) / 4) * 4;
    
    std::string label = readString(fs, 8);
    label = trim(label);
    if (rectype == 0) { // End of records
      break;
    }
    else if (rectype == 2) { // Constant
      if (recsize >= 0) { // Float value
        float val = readValue<float>(fs);
        Rcpp::NumericVector cons(1);
        cons[0] = (val != 1.23456e+30f) ? val : NA_REAL;
        if (haslabels == true && label.empty() == false) {
          cons.attr("name") = label;
        }
        constants[std::format("K{}", objid)] = cons;
      }
      else { // String value
        std::string text = readString(fs, blocksize);
        text.resize(-recsize);
        text = trim(text);
        Rcpp::StringVector cons(1);
        cons[0] = text;
        if (haslabels == true && label.empty() == false) {
          cons.attr("name") = label;
        }
        constants[std::format("K{}", objid)] = cons;
      }
    }
    else if (rectype == 3) { // Column
      if (recsize >= 0) { // Numeric Values
        recheadT numvalues = numrec / (sizeof(floatT) / sizeof(float));
        Rcpp::NumericVector values(numvalues);
        for (recheadT i = 0; i < numvalues; ++i) {
          floatT val = readValue<floatT>(fs);
          if constexpr (std::is_same_v<floatT, float>) {
            values[i] = (val != 1.23456e+30f) ? val : NA_REAL;
          }
          if constexpr (std::is_same_v<floatT, double>) {
            values[i] = (val != 1.23456e+30) ? val : NA_REAL;
          }
        }
        if (haslabels == true && label.empty() == false) {
          values.attr("name") = label;
        }
        columns[std::format("C{}", objid)] = values;
      } 
      else { // String values
        if constexpr (std::is_same_v<floatT, float>) {
          recheadT numvals = numrec / (blocksize / 4);
          Rcpp::StringVector values(numvals);
          for (std::int32_t i = 0; i < numvals; ++i) {
            std::string text = readString(fs, blocksize);
            text.resize(-recsize);
            text = trim(text);
            values[i] = text;
          }
          if (haslabels == true && label.empty() == false) {
            values.attr("name") = label;
          }
          columns[std::format("C{}", objid)] = values;
        }
        // NOTE: We don't currently now how strings are stored when double flag is on
        if constexpr (std::is_same_v<floatT, double>) {
          Rcpp::warning("String types for current worksheet version not understood, column C%i ignored", objid);
          skipBytes(fs, (blocksize / 4) * numrec);
        }
      }
    }
    else if (rectype == 4) { // Matrix
      recheadT numvalues = numrec / (sizeof(floatT) / sizeof(float));
      recheadT ncol = recsize;
      recheadT nrow = numvalues / ncol;
      Rcpp::NumericMatrix mat(nrow, ncol);
      for (recheadT i = 0; i < numvalues; ++i) {
        floatT val = readValue<floatT>(fs);
        if constexpr (std::is_same_v<floatT, float>) {
          mat[i] = (val != 1.23456e+30f) ? val : NA_REAL;
        }
        if constexpr (std::is_same_v<floatT, double>) {
          mat[i] = (val != 1.23456e+30) ? val : NA_REAL;
        }
      }
      if (haslabels == true && label.empty() == false) {
        mat.attr("name") = label;
      }    
      matrices[std::format("M{}", objid)] = mat;
    } 
    else if (rectype == 5) { // Constant (double)
      if (recsize >= 0) { // Double value
        double val = readValue<double>(fs);
        Rcpp::NumericVector cons(1);
        cons[0] = (val != 1.23456e+30) ? val : NA_REAL;
        if (haslabels == true && label.empty() == false) {
          cons.attr("name") = label;
        }
        constants[std::format("K{}", objid)] = cons;
      } 
      else { // String value
        std::string text = readString(fs, blocksize);
        text.resize(-recsize);
        text = trim(text);
        Rcpp::StringVector cons(1);
        cons[0] = text;
        if (haslabels == true && label.empty() == false) {
          cons.attr("name") = label;
        }
        constants[std::format("K{}", objid)] = cons;
      }
    } 
    else if (rectype == 100) { // Author
      if (numrec > 0) {
        author = readString(fs, sizeof(std::int32_t) * numrec);
        author = trim(author);
      }
    } 
    else if (rectype == 101) { // Date
      if (numrec > 0) {
        date = readString(fs, sizeof(std::int32_t) * numrec);
        date = trim(date);
      }
    } 
    else if (rectype == 102) { // Description
      if (numrec > 0) {
        description = readString(fs, sizeof(std::int32_t) * numrec);
        description = trim(description);
      }
    } 
    else if (rectype == 128) { // Display information
      skipBytes(fs, sizeof(std::int32_t) * numrec);
    } 
    else { // Unknown record type
      Rcpp::warning("Unrecognised object type %i", rectype);
      skipBytes(fs, (blocksize / 4) * numrec);
    }
  }
  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("constants") = constants,
    Rcpp::Named("columns") = columns,
    Rcpp::Named("matrices") = matrices
  );
  if (author.empty() == false) {
    result.attr("author") = author;
  }
  if (date.empty() == false) {
    result.attr("date") = date;
  }
  if (description.empty() == false) {
    result.attr("description") = description;
  }
  return result;
}

static Rcpp::List readOld(std::iostream &fs) {
  std::string saveinfo = readString(fs, 44);
  std::string year = readString(fs, 4);
  std::string month = readString(fs, 2);
  std::string day = readString(fs, 2);
  std::string release = readString(fs, 12);
  std::string flags = readString(fs, 16);
  
  saveinfo = trim(saveinfo);
  release = trim(release);
  year = trim(year);
  month = trim(month);
  day = trim(day);
  
  Rcpp::Rcout << std::format("{} {}", saveinfo, release) << std::endl;
  Rcpp::Rcout << std::format("{}-{}-{}", year, month, day) << std::endl;

  for (std::size_t i = 0; i < flags.size(); ++i) {
    if (flags[i] != ' ') {
      if (i == 0) {
        if (flags[i] != 'E') {
          Rcpp::warning("Unexpected flag at position %i %s", i, flags[i]);
        }
      }
      else if (i == 1) {
        if (flags[i] != 'A') {
          Rcpp::warning("Unexpected flag at position %i %s", i, flags[i]);
        }
      }
      else if (i == 2) {
        if (flags[i] != 'D') {
          Rcpp::warning("Unexpected flag at position %i %s", i, flags[i]);
        }
      }
      else if (i == 15) {
        if (flags[i] != '0' && flags[i] != '1') {
          // Not sure of the purpose of this flag
          Rcpp::warning("Unexpected flag at position %i %s", i, flags[i]);
        }
      }
      else {
        Rcpp::warning("Unexpected flag at position %i %s", i, flags[i]);
      }
    }
  }
  
  bool is32 = (flags[0] == 'E');
  bool haslabels = (flags[1] == 'A');
  bool isdouble = (flags[2] == 'D');
  
  Rcpp::List result;
  if (is32 == true) {
    if (isdouble == true) {
      result = readOld<std::int32_t, double>(fs, haslabels);
    } 
    else {
      result = readOld<std::int32_t, float>(fs, haslabels);
    }
  } 
  else {
    result = readOld<std::int16_t, float>(fs, haslabels);
  }
  return result;
}

static Rcpp::List readWorksheet(POLE::Stream &str) {
  Rcpp::List constants;
  Rcpp::List columns;
  Rcpp::List matrices;
  std::string title{};
  std::string author{};
  std::string date{};
  std::string description{};
  
  auto pos = str.tell();
  while (str.tell() < str.size()) {
    auto headstart = str.tell();
    std::uint32_t objtype = readValue<std::uint32_t>(str);
    std::uint32_t objsize = readValue<std::uint32_t>(str);
    std::uint32_t objheadsize = readValue<std::uint32_t>(str);
    if (objtype == 1) { // Storage
      if (objheadsize != str.tell() - headstart) {
        Rcpp::warning("Unexpected header %i size: expected %i got %i", objtype, objheadsize, str.tell() - headstart);
      }
      [[maybe_unused]] std::uint32_t unused = readValue<std::uint32_t>(str);
      std::string version = readString(str, 8);
      std::string platform = readString(str, 8);
      std::int32_t date = readValue<std::int32_t>(str);

      std::chrono::sys_time tp{ std::chrono::seconds{date} };
      platform = trim(platform);
      version = trim(version);

      Rcpp::Rcout << std::format("Platform: {} Version: {}", platform, version) << std::endl;
      Rcpp::Rcout << std::format("{}", tp) << std::endl;
    }
    else if (objtype == 2) { // Constant
      std::uint32_t objid = readValue<std::uint32_t>(str);
      std::uint32_t datatype = readValue<std::uint32_t>(str);
      [[maybe_unused]] std::uint32_t datasize = readValue<std::uint32_t>(str);
      std::uint32_t numvals = readValue<std::uint32_t>(str);
      std::uint32_t custinfosize = readValue<std::uint32_t>(str);
      if (custinfosize != 0) {
        Rcpp::warning("Unexpected custom info size: expected %i got 0", custinfosize);
      }
      std::string label = readString(str);
      std::string constdesc = readString(str);
      std::uint32_t fmttype = readValue<std::uint32_t>(str); // 0: General, 1: Numeric, 2: DateTime, 3: Text
      if (fmttype > 3) {
        Rcpp::warning("Unexpected format type: %i", fmttype);
      }
      std::uint32_t fmtsize = readValue< std::uint32_t>(str);
      if (fmtsize > 0) {
        // Skip format details
        skipBytes(str, fmtsize);
      }
      if (objheadsize != str.tell() - headstart) {
        Rcpp::warning("Unexpected header %i size: expected %i got %i", objtype, objheadsize, str.tell() - headstart);
      }
      if (datatype == 1) { // Numeric
        double val = readValue<double>(str);
        Rcpp::NumericVector cons(numvals);
        for (std::uint32_t i = 0; i < numvals; ++i) {
          cons[i] = (val != 1.23456e+30) ? val : NA_REAL;
        }
        if (label.empty() == false) {
          cons.attr("name") = label;
        }
        if (constdesc.empty() == false) {
          cons.attr("label") = constdesc;
        }
        constants[std::format("K{}", objid)] = cons;
      } 
      else if (datatype == 2) { // Text
        std::string text = readString(str);
        Rcpp::StringVector cons(numvals);
        for (std::uint32_t i = 0; i < numvals; ++i) {
          cons[i] = text;
        }
        if (label.empty() == false) {
          cons.attr("name") = label;
        }
        if (constdesc.empty() == false) {
          cons.attr("label") = constdesc;
        }
        constants[std::format("K{}", objid)] = cons;
      }
    }
    else if (objtype == 3) { // Column
      std::uint32_t objid = readValue<std::uint32_t>(str);
      std::uint32_t datatype = readValue<std::uint32_t>(str);
      [[maybe_unused]] std::uint32_t datasize = readValue<std::uint32_t>(str);
      std::uint32_t numvals = readValue<std::uint32_t>(str);
      std::uint32_t custinfosize = readValue<std::uint32_t>(str);
      auto custinfostart  = str.tell();
      std::uint32_t numrows = readValue<std::uint32_t>(str);
      [[maybe_unused]] std::uint32_t nummiss = readValue<std::uint32_t>(str);
      [[maybe_unused]] std::uint32_t legalsize = readValue<std::uint32_t>(str);
      std::uint32_t numlegalvals = readValue<std::uint32_t>(str);
      [[maybe_unused]] std::uint32_t ordersize = readValue<std::uint32_t>(str);
      std::uint32_t numorderingvals = readValue<std::uint32_t>(str);
      [[maybe_unused]] std::uint32_t sortorderflag = readValue<std::uint32_t>(str);
      if (custinfosize != str.tell() - custinfostart) {
        Rcpp::warning("Unexpected custom info size: expected %i got %i", custinfosize, str.tell() - custinfostart);
      }
      std::string label = readString(str);
      std::string coldesc = readString(str);
      std::uint32_t fmttype = readValue<std::uint32_t>(str); // 0: General, 1: Numeric, 2: DateTime, 3: Text
      if (fmttype > 3) {
        Rcpp::warning("Unexpected format type: %i", fmttype);
      }
      if (numrows != numvals) {
        Rcpp::warning("Column length mismatch: %i %i", numrows, numvals);
      }
      std::uint32_t fmtsize = readValue<std::uint32_t>(str);
      if (fmtsize > 0) {
        // Skip format details
        skipBytes(str, fmtsize);
      }
      if (objheadsize != str.tell() - headstart) {
        Rcpp::warning("Unexpected header %i size: expected %i got %i", objtype, objheadsize, str.tell() - headstart);
      }
      if (datatype == 1) { // Numeric
        if (fmttype == 1) { // Numeric
          Rcpp::NumericVector values(numrows);
          for (std::uint32_t i = 0; i < numrows; i++) {
            double val = readValue<double>(str);
            values[i] = (val != 1.23456e+30) ? val : NA_REAL;
          }
          if (label.empty() == false) {
            values.attr("name") = label;
          }
          if (coldesc.empty() == false) {
            values.attr("label") = coldesc;
          }
          columns[std::format("C{}", objid)] = values;
        }
        if (fmttype == 2) { // DateTime
          Rcpp::DatetimeVector values(numrows);
          for (std::uint32_t i = 0; i < numrows; i++) {
            // Convert COM/Excel DateTime to Unix epoch
            double val = (readValue<double>(str) - 25569) * 86400;
            values[i] = (val != 1.23456e+30) ? val : NA_REAL;
          }
          values.attr("tzone") = "UTC";
          if (label.empty() == false) {
            values.attr("name") = label;
          }
          if (coldesc.empty() == false) {
            values.attr("label") = coldesc;
          }
          columns[std::format("C{}", objid)] = values;
        }
        if (numlegalvals > 0) { // Read and ignore
          for (std::uint32_t i = 0; i < numlegalvals; i++) {
            [[maybe_unused]] double value = readValue<double>(str);
          }
        }
        if (numorderingvals > 0) { // Read and ignore
          for (std::uint32_t i = 0; i < numorderingvals; i++) {
            [[maybe_unused]] double value = readValue<double>(str);
          }
        }
      } 
      else if (datatype == 2) { // Text
        Rcpp::StringVector values(numrows);
        for (std::uint32_t i = 0; i < numrows; i++) {
          std::string text = readString(str);
          values[i] = text;
        }
        if (label.empty() == false) {
          values.attr("name") = label;
        }
        if (coldesc.empty() == false) {
          values.attr("label") = coldesc;
        }
        columns[std::format("C{}", objid)] = values;
        if (numlegalvals > 0) { // Read and ignore
          for (std::uint32_t i = 0; i < numlegalvals; i++) {
            std::string text = readString(str);
          }
        }
        if (numorderingvals > 0) { // Read and ignore
          for (std::uint32_t i = 0; i < numorderingvals; i++) {
            std::string text = readString(str);
          }
        }
      }
    }
    else if (objtype == 4) { // Matrix
      std::uint32_t objid = readValue<std::uint32_t>(str);
      std::uint32_t datatype = readValue<std::uint32_t>(str);
      [[maybe_unused]] std::uint32_t datasize = readValue<std::uint32_t>(str);
      std::uint32_t numvals = readValue<std::uint32_t>(str);
      std::uint32_t custinfosize = readValue<std::uint32_t>(str);
      auto custinfostart = str.tell();
      std::uint32_t numrows = readValue<std::uint32_t>(str);
      std::uint32_t numcols = readValue<std::uint32_t>(str);
      if (custinfosize != str.tell() - custinfostart) {
        Rcpp::warning("Unexpected custom info size: expected %i got %i", custinfosize, str.tell() - custinfostart);
      }
      std::string label = readString(str);
      std::string matrixdesc = readString(str);
      std::uint32_t fmttype = readValue<std::uint32_t>(str); // 0: General, 1: Numeric, 2: DateTime, 3: Text
      if (fmttype > 3) {
        Rcpp::warning("Unexpected format type: %i", fmttype);
      }
      std::uint32_t fmtsize = readValue<std::uint32_t>(str);
      if (fmtsize > 0) {
        // Skip format details
        skipBytes(str, fmtsize);
      }
      if (objheadsize != str.tell() - headstart) {
        Rcpp::warning("Unexpected header %i size: expected %i got %i", objtype, objheadsize, str.tell() - headstart);
      }
      if (datatype == 1) {
        Rcpp::NumericMatrix mat(numrows, numcols);
        for (std::uint32_t i = 0; i < numvals; ++i) {
          double val = readValue<double>(str);
          mat[i] = (val != 1.23456e+30) ? val : NA_REAL;
        }
        if (label.empty() == false) {
          mat.attr("name") = label;
        }
        if (matrixdesc.empty() == false) {
          mat.attr("label") = matrixdesc;
        }    
        matrices[std::format("M{}", objid)] = mat;
      }
    }
    else if (objtype == 6) { // Description
      if (objheadsize != str.tell() - headstart) {
        Rcpp::warning("Unexpected header %i size: expected %i got %i", objtype, objheadsize, str.tell() - headstart);
      }
      title = readString(str);
      author = readString(str);
      date = readString(str);
      description = readString(str);
    }
    else { // Not currently of interest
      // Read and ignore
      skipBytes(str, objsize - 3*sizeof(std::uint32_t));
    }
    pos += objsize;
    if (str.tell() != pos) {
      Rcpp::warning("Position mismatch: expected %i, got %i", pos, str.tell());
      str.seek(pos);
    }
  }
  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("constants") = constants,
    Rcpp::Named("columns") = columns,
    Rcpp::Named("matrices") = matrices
  );
  if (title.empty() == false) {
    result.attr("title") = title;
  }
  if (author.empty() == false) {
    result.attr("author") = author;
  }
  if (date.empty() == false) {
    result.attr("date") = date;
  }
  if (description.empty() == false) {
    result.attr("description") = description;
  }
  return result;
}

static std::tuple<std::string, std::string, std::string, bool> readWorksheetInfo(POLE::Stream &str) {
  std::string author{};
  std::string date{};
  std::string description{};
  bool altver = false;
  
  std::uint64_t size = readValue<std::uint64_t>(str);
  std::uint32_t version = readValue<std::uint32_t>(str);
  std::vector<std::uint8_t> flags(11);
  if (readRec(str, flags.data(), flags.size()) == false) return {author, date, description, altver};
  if (flags.back() != 0) { // A few example worksheets have a slightly modified structure
    altver = true;
    auto pos = str.tell();
    str.seek(pos - 1);
  }
  std::string str1 = readWString(str); // ""
  [[maybe_unused]] std::uint32_t val1 = readValue< std::uint32_t>(str); // 0
  std::string str2 = readWString(str); // ""

  if (version == 2) {
    std::uint32_t size1 = readValue<std::uint32_t>(str); // 32
    auto startpos = str.tell();
    [[maybe_unused]] std::uint32_t val2 = readValue<std::uint32_t>(str); // 4
    std::uint32_t size2 = readValue<std::uint32_t>(str); // 24
    std::vector<std::int8_t> s1(size2);
    if (readRec(str, s1.data(), s1.size()) == false) return {author, date, description, altver};
    if (size1 != str.tell() - startpos) {
      Rcpp::warning("Size mismatch: expected %i got %i", size1, str.tell() - startpos);
    }
  }

  if (size != str.tell()) {
    Rcpp::warning("Size mismatch reading WorksheetInfo: expected %i got %i", size, str.tell());
  }
  
  [[maybe_unused]] std::uint32_t maxrows = readValue<std::uint32_t>(str);
  [[maybe_unused]] std::uint32_t maxcols = readValue<std::uint32_t>(str);
  [[maybe_unused]] std::uint32_t maxconsts = readValue<std::uint32_t>(str);
  [[maybe_unused]] std::uint32_t maxmatrix = readValue<std::uint32_t>(str);
  std::uint32_t dt1 = readValue<std::uint32_t>(str);
  std::uint32_t dt2 = readValue<std::uint32_t>(str);
  double appversion = readValue<double>(str);
  double wsversion = readValue<double>(str);
  author = readWString(str);
  date = readWString(str);
  description = readWString(str);
  std::chrono::sys_time tp1{ std::chrono::seconds{dt1} };
  std::chrono::sys_time tp2{ std::chrono::seconds{dt2} };
  if (altver == false) {
    [[maybe_unused]] std::uint32_t val3 = readValue<std::uint32_t>(str);
    [[maybe_unused]] std::uint32_t val4 = readValue<std::uint32_t>(str);
  }

  Rcpp::Rcout << std::format("Worksheet version: {}, saved from version: {}", wsversion, appversion) << std::endl;
  Rcpp::Rcout << std::format("Creation date: {}, last saved date: {}", tp1, tp2) << std::endl;    
  return {author, date, description, altver};
}

static std::tuple<std::vector<std::uint32_t>, std::vector<std::uint32_t>, std::vector<std::uint32_t>, std::string > readWorksheetData(POLE::Stream& str) {
  std::vector<std::uint32_t> columns;
  std::vector<std::uint32_t> constants;
  std::vector<std::uint32_t> matrices;
  
  // File information
  std::uint64_t size = readValue<std::uint64_t>(str);
  [[maybe_unused]] std::uint32_t val1 = readValue<std::uint32_t>(str);
  [[maybe_unused]] std::uint32_t val2 = readValue<std::uint32_t>(str);
  std::string work = readString(str, 4);
  if (work != "WORK") {
    Rcpp::warning("Unexpected string: expected WORK, got %s", work);
  }
  [[maybe_unused]] std::uint32_t val3 = readValue<std::uint32_t>(str);
  std::string path = readWString(str);
  std::string title = readWString(str);
  Rcpp::Rcout << std::format("Original path: {}", path) << std::endl;
  
  // Sub-header
  auto substart = str.tell();
  std::uint64_t subsize = readValue<std::uint64_t>(str);
  std::uint32_t subversion = readValue<std::uint32_t>(str); // 1
  if (subversion != 1) {
    Rcpp::warning("Unexpected value: expected 1, got %i", subversion);
  }
  std::string str1 = readWString(str);
  if (subsize != str.tell() - substart) {
    Rcpp::warning("Size mismath in WorksheetData: expected %i, got %i", subsize, str.tell() - substart);
  }
  
  // Display information (skip)
  std::uint64_t dispsize = readValue<std::uint64_t>(str);
  skipBytes(str, dispsize - sizeof(std::uint64_t));
  
  std::uint8_t val4 = readValue<std::uint8_t>(str); //1
  if (val4 != 1) {
    Rcpp::warning("Unexpected value: expected 1, got %i", val4);
  }
  std::uint32_t numcolumns = readValue<std::uint32_t>(str);
  for (std::uint32_t i = 0; i < numcolumns; ++i) {
    auto [addr, objclass, objid] = readAddr(str);
    columns.push_back(addr);
  }
  std::uint32_t numconsts = readValue<std::uint32_t>(str);
  for (std::uint32_t i = 0; i < numconsts; ++i) {
    auto [addr, objclass, objid] = readAddr(str);
    constants.push_back(addr);
  }
  std::uint32_t nummatrix = readValue<std::uint32_t>(str);
  for (std::uint32_t i = 0; i < nummatrix; ++i) {
    auto [addr, objclass, objid] = readAddr(str);
    matrices.push_back(addr);
  }
  [[maybe_unused]] std::uint8_t designspecified = readValue<std::uint8_t>(str); // 0, 1 - Design of experiment data present
  std::uint32_t designspecificationsize = readValue<std::uint32_t>(str);
  if (designspecificationsize > 0) { // Design details
    skipBytes(str, designspecificationsize);
  }
  
  // Other linked object (formula/models, etc) information (ignore)
  std::uint32_t linksize = readValue<std::uint32_t>(str);
  if (linksize > 0) {
    skipBytes(str, linksize);
  }
  
  if (size != str.tell()) {
    Rcpp::warning("Unexpected WorksheetData size: expected %i, got %i", size, str.tell());
  }
  return  { columns, constants, matrices, title };
}

static std::tuple<std::uint32_t, std::uint32_t, std::uint32_t> readContentInfo(POLE::Stream& str) {
  std::uint64_t size = readValue<std::uint64_t>(str);
  std::uint32_t version = readValue<std::uint32_t>(str); // 1
  if (version != 1) {
    Rcpp::warning("Unexpected value: expected 1, got %i", version);
  }
  [[maybe_unused]] std::uint32_t val1 = readValue<std::uint32_t>(str); // 1
  std::uint8_t empty = readValue<std::uint8_t>(str); // 0, 1
  auto substart = str.tell();
  std::uint64_t subsize = readValue<std::uint64_t>(str);
  std::uint32_t subversion = readValue<std::uint32_t>(str); // 1
  if (subversion != 1) {
    Rcpp::warning("Unexpected version: expected 1 got %i", subversion);
  }
  std::uint32_t streamtype = readValue<std::uint32_t>(str); // 2: constant 3: column 4: matrix

  [[maybe_unused]] std::uint8_t val2 = readValue<std::uint8_t>(str); // 0
  [[maybe_unused]] std::uint8_t val3 = readValue<std::uint8_t>(str); // 1
  std::uint32_t dataid = readValue<std::uint32_t>(str);
  if (subsize != str.tell() - substart) {
    Rcpp::warning("Unexpected size in ContentInfo: expected %i got %i", subsize, str.tell() - substart);
  }
  
  std::uint32_t coladdr = 0;
  if (empty == 0) {
    auto [addr, objclass, objid] = readAddr(str); // addr, 437, CmWorksheetVarBody
    coladdr = addr;
  }
  auto [addr2, objclass2, objid2] = readAddr(str); // 0, 0, ""
  if (size != str.tell()) {
    Rcpp::warning("Unexpected size reading ContentInfo: expected %i got %i", size, str.tell());
  }
  
  return { coladdr, streamtype, dataid };
}

static Rcpp::RObject readContentData(POLE::Stream& str, bool altver, std::uint32_t contenttype) {
  Rcpp::RObject content;
  
  std::uint64_t size = readValue<std::uint64_t>(str);
  std::uint32_t version = readValue<std::uint32_t>(str); // 1
  if (version != 1) {
    Rcpp::warning("Unexpected value: expected 1, got %i", version);
  }
  std::string label = readWString(str);
  std::string description = readWString(str);
  std::uint32_t fmttype = readValue<std::uint32_t>(str); // 0: Empty, 1: Numeric, 2: Date 3: Text
  if (fmttype != 0) {
    std::uint64_t fmtsize = readValue<std::uint64_t>(str);
    // Ignore/skip formatting specification
    skipBytes(str, fmtsize - sizeof(std::uint64_t));
  }

  auto datastart = str.tell();
  std::uint64_t datasize = readValue<std::uint64_t>(str);
  std::uint32_t recver = readValue<std::uint32_t>(str); // 1 for WorksheetData, 2 for later versions

  if (altver == false) {
    // Sometimes missing but should equal contenttype and therefore recoverable
    std::uint32_t streamtype = readValue<std::uint32_t>(str); // 2: constant 3: column 4: matrix
    if (streamtype != contenttype) {
      Rcpp::warning("Mismatched stream type");
    }
  }
  
  std::uint32_t datatype = readValue<std::uint32_t>(str); // 0: empty, 1: numeric/date, 2: text
  if (datatype > 2) {
    Rcpp::warning("Unexpected data type: %i", datatype);
  }

  auto subdatastart = str.tell();
  std::uint64_t subdatasize = readValue<std::uint64_t>(str);
  std::uint32_t subdataversion = readValue< std::uint32_t>(str); // 1
  if (subdataversion != 1) {
    Rcpp::warning("Unexpected version: expected 1, got %i", subdataversion);
  }
  [[maybe_unused]] std::uint32_t numvals = readValue<std::uint32_t>(str);
  std::uint32_t numcols = readValue<std::uint32_t>(str);
  [[maybe_unused]] std::uint32_t max_width = readValue<std::uint32_t>(str);
  std::uint8_t isnumeric = readValue<std::uint8_t>(str); // 0 if text, 1 if numeric/date
  [[maybe_unused]] std::uint8_t istext = (fmttype == 3) ? readValue<std::uint8_t>(str) : 0;
  std::uint32_t numvalue = (numcols > 0) ? readValue<std::uint32_t>(str) : 0;

  if (contenttype == 2) { // Constant
    if (isnumeric == 1) {
      Rcpp::NumericVector cons(numvalue);
      for (std::uint32_t i = 0; i < numvalue; ++i) {
        double val = readValue<double>(str);
        cons[i]= (val != 1.23456e+30) ? val : NA_REAL;
      }
      if (label.empty() == false) {
        cons.attr("name") = label;
      }
      if (description.empty() == false) {
        cons.attr("label") = description;
      }
      content = cons;
    }
    else {
      Rcpp::StringVector cons(numvalue);
      for (std::uint32_t i = 0; i < numvalue; ++i) {
        std::string text = readWString(str);
        cons[i] = text;
      }
      if (label.empty() == false) {
        cons.attr("name") = label;
      }
      if (description.empty() == false) {
        cons.attr("label") = description;
      }
      content = cons;
    }
  }
  else if (contenttype == 3) { // Column
    if (isnumeric == 1) { // Text
      if (fmttype == 1) { // Numeric
        Rcpp::NumericVector values(numvalue);
        for (std::uint32_t i = 0; i < numvalue; ++i) {
          double val = readValue<double>(str);
          values[i] = (val != 1.23456e+30) ? val : NA_REAL;
        }
        if (label.empty() == false) {
          values.attr("name") = label;
        }
        if (description.empty() == false) {
          values.attr("label") = description;
        }
        content = values;
      }
      if (fmttype == 2) { // DateTime
        Rcpp::DatetimeVector values(numvalue);
        for (std::uint32_t i = 0; i < numvalue; ++i) {
          // Convert COM/Excel DateTime to Unix epoch
          double val = (readValue<double>(str) - 25569) * 86400;
          values[i] = (val != 1.23456e+30) ? val : NA_REAL;
        }
        values.attr("tzone") = "UTC";
        if (label.empty() == false) {
          values.attr("name") = label;
        }
        if (description.empty() == false) {
          values.attr("label") = description;
        }
        content = values;
      }
    }
    else { // Text
      Rcpp::StringVector values(numvalue);
      for (std::uint32_t i = 0; i < numvalue; ++i) {
        std::string text = readWString(str);
        values[i] = text;
      }
      if (label.empty() == false) {
        values.attr("name") = label;
      }
      if (description.empty() == false) {
        values.attr("label") = description;
      }
      content = values;
    }
  }
  else if (contenttype == 4) { // Matrix
    if (isnumeric == 1) {
      std::uint32_t numrows = numvalue / numcols;
      Rcpp::NumericMatrix mat(numrows, numcols);
      for (std::uint32_t i = 0; i < numvalue; ++i) {
        double val = readValue<double>(str);
        mat[i] = (val != 1.23456e+30) ? val : NA_REAL;
      }
      if (label.empty() == false) {
        mat.attr("name") = label;
      }
      if (description.empty() == false) {
        mat.attr("label") = description;
      }
      content = mat;
    }
  }
  
  if (fmttype != 3) {
    std::uint8_t pad = readValue<std::uint8_t>(str); // 0
    if (pad != 0) {
      Rcpp::warning("Unexpected value: expected 0, got %i", pad);
    }
  }
  if (subdatasize != str.tell() - subdatastart) {
    Rcpp::warning("Unexpected subdata size: expected %i, got %i", subdatasize, str.tell() - subdatastart);
  }
  if (recver == 2) {
    [[maybe_unused]] std::uint32_t extra = readValue<std::uint32_t>(str);
    [[maybe_unused]] std::uint8_t hasformula = readValue<std::uint8_t>(str);
  }
  
  if (datasize != str.tell() - datastart) {
    Rcpp::warning("Unexpected data size: expected %i, got %i", datasize, str.tell() - datastart);
  }

  // Order block
  auto ordstart = str.tell();
  std::uint64_t ordsize = readValue<std::uint64_t>(str);
  std::uint32_t ordversion = readValue<std::uint32_t>(str); // 1
  if (ordversion != 1) {
    Rcpp::warning("Unexpected version: %i", ordversion);
  }
  std::uint32_t ordtype = readValue<std::uint32_t>(str); // 0: value order is alphabetic, 1 : value order is occurrence 2 : value order is user specified with values following
  if (ordtype > 2) {
    Rcpp::warning("Unexpected order type: %i", ordtype);
  }
  std::uint32_t orddir = readValue<std::uint32_t>(str); // 0 (order direction?)
  if (orddir != 0 && orddir != 1) {
    Rcpp::warning("Unexpected order value: %i", orddir);
  }
  std::uint32_t ordrecs = readValue<std::uint32_t>(str);
  for (std::uint32_t i = 0; i < ordrecs; ++i) {
    std::string ordstr = readWString(str);
    [[maybe_unused]] double ordval = readValue<double>(str);
  }
  if (str.tell() - ordstart != ordsize) {
    Rcpp::warning("Unexpected order block size: expected %i got %i", ordsize, str.tell() - ordstart);
  }
  
  // Display information (ignore and skip)
  std::uint32_t displaytype = readValue<std::uint32_t>(str); // 3: column, 0: otherwise
  if (displaytype != 0 && displaytype != 3) {
    Rcpp::warning("Unexpected type: %i", displaytype);
  }
  std::uint64_t displayinfosize = readValue<std::uint64_t>(str);
  skipBytes(str, displayinfosize - sizeof(std::uint64_t));

  std::vector<std::uint8_t> flags2(4);
  if (readRec(str, flags2.data(), flags2.size()) == false) return content;
  
  if (size != str.tell()) {
    Rcpp::warning("Unexpected ContentData size: expected %i got %i", size, str.tell());
  }
  
  return content;
}

static Rcpp::List readOle(std::iostream &fs) {
  Rcpp::List result;
  POLE::Storage storage(&fs);
  storage.open();
  if (storage.result() == POLE::Storage::Ok) {
    auto entries = storage.entries("/");

    for (const auto &lab : entries) {
      if (lab == "Worksheet") {
        POLE::Stream str(&storage, lab);
        if (str.fail()) {
          Rcpp::warning("Failed to open the Worksheet stream");
          continue;
        }
        result = readWorksheet(str);
      } else if (lab == "WorksheetData" || 
          lab == "WorksheetData15" || 
          lab == "WorksheetData16" || 
          lab == "WorksheetData17" || 
          lab == "WorksheetDataPresent") {
        
        POLE::Stream infostr(&storage, "WorksheetInfo");
        if (infostr.fail()) {
          Rcpp::warning("Failed to open the WorksheetInfo stream");
          continue;
        }
        auto [author, date, description, altver] = readWorksheetInfo(infostr);
  
        POLE::Stream datastr(&storage, lab);
        if (datastr.fail()) {
          Rcpp::warning("Failed to open the %s stream", lab);
          continue;
        }
  
        auto [columnaddrs, constantaddrs, matrixaddrs, title] = readWorksheetData(datastr);
        
        POLE::Stream compstream(&storage, "Compressed/Data");
        if (compstream.fail()) {
          Rcpp::warning("Failed to open the Compressed/Data stream");
          continue;
        }
        std::uint32_t level = readValue<std::uint32_t>(compstream); // 4
        if (level != 4) {
          Rcpp::warning("Unexpected compression level: expected 4, got %i", level);
        }
        std::uint32_t compsize = readValue<std::uint32_t>(compstream);
        std::vector<unsigned char> compressed(compsize);
        if (readRec(compstream, compressed.data(), compressed.size()) == false) break;
        
        ARJDecompress unarj(compressed);
        auto& decompresssed = unarj.data();
        POLE::Storage compstorage(&decompresssed);
        compstorage.open();
        if (compstorage.result() == POLE::Storage::Ok) {
          POLE::Stream keystream(&compstorage, "/Shared/Key");
          if (keystream.fail()) {
            Rcpp::warning("Failed to open the Shared/Key stream");
            continue;
          }
          std::unordered_map<std::uint32_t, std::uint32_t> map;
          std::size_t numentries = keystream.size() / 8;
          for (std::size_t i = 0; i < numentries; ++i) {
            std::uint32_t key = readValue<std::uint32_t>(keystream);
            std::uint32_t value = readValue<std::uint32_t>(keystream);
            map[key] = value;
          }

          Rcpp::List columns;
          for (const auto &infoaddr : columnaddrs) {
            std::uint32_t objinfoid = map[infoaddr];
            POLE::Stream infostream(&compstorage, "/Shared/Object" + std::to_string(objinfoid));
            if (infostream.fail()) {
              Rcpp::warning("Failed to open the Shared/Object%i stream", objinfoid);
              continue;
            }
            auto [contentaddr, contentype, contentid] = readContentInfo(infostream);
            if (contentaddr != 0) {
              std::uint32_t objcontentid = map[contentaddr];
              POLE::Stream contentstream(&compstorage, "/Shared/Object" + std::to_string(objcontentid));
              if (contentstream.fail()) {
                Rcpp::warning("Failed to open the Shared/Object%i stream", objcontentid);
                continue;
              }
              columns[std::format("C{}", contentid)] = readContentData(contentstream, altver, contentype);
            }
          }
          
          Rcpp::List constants;
          for (const auto &infoaddr : constantaddrs) {
            std::uint32_t objinfoid = map[infoaddr];
            POLE::Stream infostream(&compstorage, "/Shared/Object" + std::to_string(objinfoid));
            if (infostream.fail()) {
              Rcpp::warning("Failed to open the Shared/Object%i stream", objinfoid);
              continue;
            }
            auto [contentaddr, contentype, contentid] = readContentInfo(infostream);
            if (contentaddr != 0) {
              std::uint32_t objcontentid = map[contentaddr];
              POLE::Stream contentstream(&compstorage, "/Shared/Object" + std::to_string(objcontentid));
              if (contentstream.fail()) {
                Rcpp::warning("Failed to open the Shared/Object%i stream", objcontentid);
                continue;
              }
              constants[std::format("K{}", contentid)] = readContentData(contentstream, altver, contentype);
            }
          }
          
          Rcpp::List matrices;
          for (const auto &infoaddr : matrixaddrs) {
            std::uint32_t objinfoid = map[infoaddr];
            POLE::Stream infostream(&compstorage, "/Shared/Object" + std::to_string(objinfoid));
            if (infostream.fail()) {
              Rcpp::warning("Failed to open the Shared/Object%i stream", objinfoid);
              continue;
            }
            auto [contentaddr, contentype, contentid] = readContentInfo(infostream);
            if (contentaddr != 0) {
              std::uint32_t objcontentid = map[contentaddr];
              POLE::Stream contentstream(&compstorage, "/Shared/Object" + std::to_string(objcontentid));
              if (contentstream.fail()) {
                Rcpp::warning("Failed to open the Shared/Object%i stream", objcontentid);
                continue;
              }
              matrices[std::format("M{}", contentid)] = readContentData(contentstream, altver, contentype);
            }
          }
          result["constants"] = constants;
          result["columns"] = columns;
          result["matrices"] = matrices;
          if (title.empty() == false) {
            result.attr("title") = title;
          }
          if (author.empty() == false) {
            result.attr("author") = author;
          }
          if (date.empty() == false) {
            result.attr("date") = date;
          }
          if (description.empty() == false) {
            result.attr("description") = description;
          }
        }
      }
    }
  }
  return result;
}

//' rcpp_readmtw
//' 
//' This function allows reading data from a variety of historic Minitab formats
//' 
//' @param filename name of file to open
//' @return A list containing a lists of constants, matrices and columns that are
//' stored in the worksheet.
//' @export
// [[Rcpp::export]]
Rcpp::List rcpp_readmtw(std::string filename) {
  Rcpp::List result;
  std::fstream fs(filename, std::ios::in | std::ifstream::binary);
  if (!fs.good()) {
    Rcpp::stop("Unable to open file %s\n", filename);
  }
  std::string sig(8, ' ');
  fs.read(sig.data(), sig.size());
  fs.seekg(0);
  if (sig == "WORKSHEE") {
    result = readOld(fs);
  } 
  else if (sig == "\xD0\xCF\x11\xE0\xA1\xB1\x1A\xE1") {
    result = readOle(fs);
  } 
  else {
    Rcpp::stop("Unrecognised file type\n");
  }
  result.attr("class") = "MTWorksheet";
  return result;
}
