#include <Rcpp.h>

#include <cstdint>
#include <format>
#include <fstream>
#include <string>

[[nodiscard]] static std::string_view trim(std::string_view str) {
  str.remove_prefix(std::min(str.find_first_not_of(" "), str.size()));
  auto end_pos = str.find_last_not_of(" ");
  if (end_pos != std::string_view::npos) {
    str.remove_suffix(str.size() - end_pos - 1);
  }
  return str;
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

class RecordReader {
public:
  RecordReader(std::istream& fs, int recsize, int numrecs) : fs(fs), recsize(recsize), recsleft(numrecs) {}
  [[nodiscard]] std::string nextRec() {
    if (pos + recsize > 80) {
      if (recsleft != 0) {
        std::getline(fs, buffer);
      } 
      else {
        Rcpp::warning("Attempted to read past the end of a record");
      }
      pos = 0;
    }
    
    std::string result = buffer.substr(pos, recsize);
    result = ANSI2UTF8(result);
    --recsleft;
    pos += recsize;
    
    return result;
  }
private:
  std::istream& fs;
  int recsize;
  int recsleft;
  std::string buffer;
  int pos = 80;
};

static Rcpp::List readRecs(std::iostream &fs) {
  Rcpp::List constants;
  Rcpp::List columns;
  Rcpp::List matrices;
  std::string author{};
  std::string date{};
  std::string description{};
  while (!fs.eof()) {
    std::string rechead;
    std::getline(fs, rechead);
    if (rechead == "") {
      // End of file
      break;
    }
    if (rechead.front() != '%' || rechead.back() != '.') {
      Rcpp::warning("Unexpected record structure");
      break;
    }
    std::int32_t objtype = std::stoi(rechead.substr(1, 7));
    std::int32_t objid = std::stoi(rechead.substr(8, 7));
    // Number of blocks total
    std::int32_t nrec = std::stoi(rechead.substr(16, 7));
    // numeric records: Number of missing values
    // string records: -block size (= maximum string length + 1)
    // descriptive records: number of lines
    std::int32_t size = std::stoi(rechead.substr(24, 7));
    std::string label = rechead.substr(29, 50);
    label = ANSI2UTF8(label);
    label = trim(label);
    std::int32_t recsize = size < 0 ? -size + 1 : 16;
    RecordReader datarecs(fs, recsize, nrec);
    
    if (objtype == 2) { // Constant (numeric is single precision)
      if (nrec > 0) {
        std::string val = datarecs.nextRec();
        val = trim(val);
        if (size >= 0) { // Numeric
          Rcpp::NumericVector cons(1);
          if (val != "*") {
            std::size_t endpos{};
            cons[0] = std::stod(val, &endpos);
            if (endpos != val.length()) {
              Rcpp::warning("Numeric value not fully parsed %s", val);
            }
          }
          else {
            cons[0] = NA_REAL;
          }
          if (label.empty() == false) {
            cons.attr("name") = label;
          }
          constants[std::format("K{}", objid)] = cons;
        }
        else { // Text
          Rcpp::StringVector values(nrec);
          values[0] = val;
          if (label.empty() == false) {
            values.attr("name") = label;
          }
          columns[std::format("K{}", objid)] = values;
        }
      }
    }
    else if (objtype == 3) { // Column
      if (size >= 0) { // Numeric
        Rcpp::NumericVector values(nrec);
        for (std::int32_t i = 0; i < nrec; ++i) {
          std::string val = datarecs.nextRec();
          val = trim(val);
          if (val != "*") {
            std::size_t endpos{};
            values[i] = std::stod(val, &endpos);
            if (endpos != val.length()) {
              Rcpp::warning("Numeric value not fully parsed %s", val);
            }
          }
          else {
            values[i] = NA_REAL;
          }
        }
        if (label.empty() == false) {
          values.attr("name") = label;
        }
        columns[std::format("C{}", objid)] = values;
      }
      else { // Text
        Rcpp::StringVector values(nrec);
        for (std::int32_t i = 0; i < nrec; ++i) {
          std::string text = datarecs.nextRec();
          text.resize(-size);
          text = trim(text);
          values[i] = text;
        }
        if (label.empty() == false) {
          values.attr("name") = label;
        }
        columns[std::format("C{}", objid)] = values;
      }
    }
    else if (objtype == 4) { // Matrix
      std::int32_t ncol = size;
      std::int32_t  nrow = nrec / size;
      Rcpp::NumericMatrix mat(nrow, ncol);
      for (std::int32_t i = 0; i < nrec; ++i) {
        std::string val = datarecs.nextRec();
        val = trim(val);
        if (val != "*") {
          std::size_t endpos{};
          mat[i] = std::stod(val, &endpos);
          if (endpos != val.length()) {
            Rcpp::warning("Numeric value not fully parsed %s", val);
          }
        }
        else {
          mat[i] = NA_REAL;
        }
      }
      if (label.empty() == false) {
        mat.attr("name") = label;
      }    
      matrices[std::format("M{}", objid)] = mat;
    }
    else if (objtype == 5) { // Constant (numeric is double-precision)
      std::string val = "";
      for (int i = 0; i < nrec; ++i) {
        val += datarecs.nextRec();
      }
      val = trim(val);
      if (size >= 0) { // Numeric
        Rcpp::NumericVector cons(1);
        if (val != "*") {
          std::size_t endpos = 0;
          cons[0] = std::stod(val, &endpos);
          if (endpos != val.length()) {
            Rcpp::warning("Numeric value not fully parsed %s", val);
          }
        }
        else {
          cons[0] = NA_REAL;
        }
        if (label.empty() == false) {
          cons.attr("name") = label;
        }
        constants[std::format("K{}", objid)] = cons;
      }
      else { // Text
        Rcpp::StringVector values(1);
        values[0] = val;
        if (label.empty() == false) {
          values.attr("name") = label;
        }
        columns[std::format("K{}", objid)] = values;
      }
    }
    else if (objtype == 100) { // Author
      for (std::int32_t i = 0; i < nrec; ++i) {
        author += datarecs.nextRec();
      }
      author = trim(author);
    }
    else if (objtype == 101) { // Date
      for (std::int32_t i = 0; i < nrec; ++i) {
        date += datarecs.nextRec();
      }
      date = trim(date);
    }
    else if (objtype == 102) { // Description
      for (std::int32_t i = 0; i < nrec; ++i) {
        description += datarecs.nextRec();
      }
      std::size_t pos = 0;
      while (pos != description.npos) {
        pos = description.find("\x1F\x1E");
        if (pos != description.npos) {
          description.replace(pos, 2, "\n");
        }
      }
      description = trim(description);
    }
    else {
      Rcpp::warning("Unknown record type %i", objtype);
      for (std::int32_t i = 0; i < nrec; ++i) {
        [[maybe_unused]] std::string unknown = datarecs.nextRec();
      }
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

//' rcpp_readmtp
//' 
//' This function allows reading data the minitab portable file format (MTP)
//' 
//' @param filename name of file to open
//' @return A list containing a lists of constants, matrices and columns that are
//' stored in the worksheet.
//' @export
// [[Rcpp::export]]
Rcpp::List rcpp_readmtp(std::string filename) {
  Rcpp::List result;

  std::fstream fs(filename, std::ios::in);
  if (!fs.good()) {
    Rcpp::stop("Unable to open file %s\n", filename);
  }
  std::string filehead;
  std::getline(fs, filehead);
  if (filehead.starts_with("Minitab Portable Worksheet")) {
    result = readRecs(fs);
  } 
  else {
    Rcpp::stop("Unrecognised file type\n");
  }
  result.attr("class") = "MTWorksheet";
  return result;
}

