[![MELPA](https://melpa.org/packages/vhdl-ts-mode-badge.svg)](https://melpa.org/#/vhdl-ts-mode)
[![MELPA Stable](https://stable.melpa.org/packages/vhdl-ts-mode-badge.svg)](https://stable.melpa.org/#/vhdl-ts-mode)
[![Build Status](https://github.com/gmlarumbe/vhdl-ts-mode/actions/workflows/build_straight.yml/badge.svg)](https://github.com/gmlarumbe/vhdl-ts-mode/actions/workflows/build_straight.yml)
[![Build Status](https://github.com/gmlarumbe/vhdl-ts-mode/actions/workflows/build_package_melpa_basic.yml/badge.svg)](https://github.com/gmlarumbe/vhdl-ts-mode/actions/workflows/build_package_melpa_basic.yml)
[![Build Status](https://github.com/gmlarumbe/vhdl-ts-mode/actions/workflows/build_package_melpa_stable.yml/badge.svg)](https://github.com/gmlarumbe/vhdl-ts-mode/actions/workflows/build_package_melpa_stable.yml)


# vhdl-ts-mode.el - VHDL Tree-sitter mode for Emacs #

The package `vhdl-ts-mode` provides syntax highlighting,
indentation, `imenu`, `which-func`, navigation and basic beautify and completion features.

`vhdl-ts-mode` is derived from `vhdl-mode` making beautify and other utilities still available.


## Requirements ##

- Emacs 29.1+ with tree-sitter support
- VHDL tree-sitter grammar

Before installing/building Emacs make sure that tree-sitter is available:

* Ubuntu:
``` shell
$ sudo apt-get install tree-sitter
```
* Arch:
``` shell
$ sudo pacman -S tree-sitter
```
* Manually:
```shell
$ git clone https://github.com/tree-sitter/tree-sitter.git
$ cd tree-sitter
$ make && sudo make install
```

If Emacs has been built with tree-sitter support the following command should return `t`:
```elisp
(treesit-available-p)
```

## Installation ##

### MELPA ###

`vhdl-ts-mode` is available on MELPA.

### straight.el ###

To install it via [straight](https://github.com/radian-software/straight.el) with `use-package`:

```emacs-lisp
(straight-use-package 'use-package)
(use-package vhdl-ts-mode)
```

### Tree-sitter grammar ###

The package provides an interactive command to simplify the installation of the grammar:

- `M-x RET vhdl-ts-install-grammar RET`

This command requires Git, a C compiler and (sometimes) a C++ compiler,
and the linker to be installed and on the PATH.


## Setup ##

To open VHDL files with `vhdl-ts-mode` simply add this line to your init file:

``` elisp
(add-to-list 'auto-mode-alist '("\\.vhdl?\\'" . vhdl-ts-mode))
```

### Syntax highlighting ###

To change the faces default values there are two methods:

* Via <kbd>M-x</kbd> `customize-group` <kbd>RET</kbd> `vhdl-ts-faces`.
   - Customize faces and save configuration once you get the desired result

* Through elisp code
   - Below there is a snippet with a configuration example that works well with a dark background:
    ``` elisp
  (set-face-attribute 'vhdl-ts-font-lock-then-face nil :foreground "dark olive green")
  (set-face-attribute 'vhdl-ts-font-lock-punctuation-face nil :foreground "burlywood")
  (set-face-attribute 'vhdl-ts-font-lock-operator-face nil :inherit 'vhdl-ts-font-lock-punctuation-face :weight 'extra-bold)
  (set-face-attribute 'vhdl-ts-font-lock-parenthesis-face nil :foreground "dark goldenrod")
  (set-face-attribute 'vhdl-ts-font-lock-brackets-content-face nil :foreground "yellow green")
  (set-face-attribute 'vhdl-ts-font-lock-port-connection-face nil :foreground "bisque2")
  (set-face-attribute 'vhdl-ts-font-lock-entity-face nil :foreground "green1")
  (set-face-attribute 'vhdl-ts-font-lock-instance-face nil :foreground "medium spring green")
  (set-face-attribute 'vhdl-ts-font-lock-instance-lib-face nil :foreground "gray70")
  (set-face-attribute 'vhdl-ts-font-lock-translate-off-face nil :background "gray20" :slant 'italic)
    ```


# Contributing #

Contributions are welcome! Just stick to common Elisp conventions and run the ERT suite after testing your changes and before submitting a new PR.

For new functionality add new ERT tests if possible.

Consider [sponsoring](https://github.com/sponsors/gmlarumbe) to help
maintaining the project and for the development of new features. *Thank you!*

### Setup ###

To run the whole ERT test suite change directory to the `vhdl-ts-mode`
root and make sure `test-hdl` Git submodule has been loaded:

```shell
git submodule update --init
```

### Targets ###

Then run the default target:

```shell
$ make
```

To run a subset of tests (e.g. navigation):

```shell
$ make TESTS=navigation
```

To regenerate all the expected outputs for the tests:

```shell
$ make gen
```

To regenerate the expected outputs for a group of tests (e.g. navigation):

```shell
$ make gen TESTS=navigation
```

## Other Emacs packages
* [verilog-ts-mode](https://github.com/gmlarumbe/verilog-ts-mode): SystemVerilog Tree-sitter mode
* [verilog-ext](https://github.com/gmlarumbe/verilog-ext): SystemVerilog Extensions
* [vhdl-ext](https://github.com/gmlarumbe/vhdl-ext): VHDL Extensions
* [fpga](https://github.com/gmlarumbe/fpga): FPGA & ASIC Utilities for tools of major vendors and open source
* [wavedrom-mode](https://github.com/gmlarumbe/wavedrom-mode): edit and render WaveJSON files to create timing diagrams
* [vunit-mode](https://github.com/embed-me/vunit-mode.git): Integration of [VUnit](https://github.com/VUnit/vunit) workflow
