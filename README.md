# ExchangeRates
Loads the current exchange rates for all countries from a webservice and displays them in a grid.

The exchange rates are downloaded from https://openexchangerates.org/. An App_ID is required to be able to access this service; it must be entered upon first usage of the program and will be stored for later usage in an ini file. The App_ID can be received after registration at https://openexchangerates.org/signup; the "Free Plan" normally is sufficient which allows for 1000 requests per months.

![Screenshot](images/screenshot-v0.png)

The list of countries and their currencies is provided by https://www.six-group.com/en/products-services/financial-information/data-standards.html#scrollTo=currency-codes (date: Oct 1, 2021)

# Installation
This repository contains Lazarus source files only. Please use Lazarus v2.0 or later to compile the binary.

No additional packages are required to compile the application. But the OpenSSL library must be available. In Windows, copy the files libeay32.dll and ssleay32.dll of the correct bitness to the application directory.

For Windows the OpenSSL dlls can be downloaded from

    32 bit: https://packages.lazarus-ide.org/openssl-1.0.2j-i386-win32.zip
    64 bit: https://packages.lazarus-ide.org/openssl-1.0.2j-x64_86-win64.zip
