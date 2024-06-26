[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/distrap/tower/ci.yaml?branch=main)](https://github.com/distrap/tower/actions/workflows/ci.yaml)

# [Tower][tower]

## About

Tower is a concurrency framework for the [Ivory language][ivory]. Tower
composes Ivory programs into monitors which communicate with synchronous
channels.

Tower uses pluggable backends to support individual operating systems and
target architectures. A backend for the [FreeRTOS][freertos] operating
system running on the [STM32][] line of microcontrollers is available in
the [ivory-tower-stm32][] repo, and a backend for general purpose POSIX
operating systems is available in the [ivory-tower-posix][] repo.

## Copyright and license

Copyright 2017-2022, distrap.org
Copyright 2015 [Galois, Inc.][galois]

Licensed under the BSD 3-Clause License; you may not use this work except in
compliance with the License. A copy of the License is included in the LICENSE
file.

Portions Copyright (c) 2013-2014, Spiros Eliopoulos, derived from the now
unmaintained `toml` package.

[ivory]: http://github.com/distrap/ivory
[tower]: http://github.com/distrap/tower
[ivory-tower-stm32]: http://github.com/distrap/ivory-tower-stm32
[ivory-tower-posix]: http://github.com/distrap/ivory-tower-posix
[overview]: http://smaccmpilot.org/software/tower-overview.html

[STM32]: http://www.st.com/stm32
[freertos]: http://freertos.org
[galois]: http://galois.com
