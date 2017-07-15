# getMyIp

[![Travis-CI Build Status](https://travis-ci.org/chiefBiiko/getMyIp.svg?branch=master)](https://travis-ci.org/chiefBiiko/getMyIp) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/chiefBiiko/getMyIp?branch=master&svg=true)](https://ci.appveyor.com/project/chiefBiiko/getMyIp) [![Coverage Status](https://img.shields.io/codecov/c/github/chiefBiiko/getMyIp/master.svg)](https://codecov.io/github/chiefBiiko/getMyIp?branch=master)

***

Get your public and private ipv4.

***

## Get it

```r
devtools::install_github('chiefBiiko/getMyIp')
```

***

## Usage

```r
getMyIp::publicV4()   # '22.36.44.77'
getMyIp::privateV4()  # '192.168.178.22'
getMyIp::listV4()     # named list: $public, $private
```

***

## License

MIT
