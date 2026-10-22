# readmtw
R package to read data from historic .mtw worksheet files

`readMTW()` is an R function to allow reading data from the historic `.mtw` file format

This function reads constants, matrices and columns from files saved in the `.mtw`
format that was used in Minitab prior to release 20. 

Formats corresponding to release 5 to 19 should be handled, with the exception of 
string formats for release 11 where values were first stored in double-precision.
 
Subsequent versions have switched to the .mwx format, which is not supported by 
this package. This consists of JSON formatted files stored in a zip container.

Support for structured storage is provided by the POLE library (BSD license)
Support for decompressing the ARJ format is based on code from the pyembroidery 
project (MIT license)

`readMTP()` is an R function to allow reading data from the `.mtp` portable file format

This function reads constants, matrices and columns from files saved in the `.mtp`
format. This handles changes introduced to the format since support was originally
provided by the foreign package.
 
Disclaimer:
The details of these formats were mostly determined through the examination of
sample worksheets freely available on the Internet. This is therefore possible
that variants exist that have not been examined and therefore are not correctly
handled. Please take care to check any data after loading it.