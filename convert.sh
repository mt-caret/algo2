#!/usr/bin/env bash
set -e

# Project Gutenbergより取得
# $ wget https://www.gutenberg.org/ebooks/1661.txt.utf-8

cat 1661.txt.utf-8 |
tail -n +55 |
head -n -370
