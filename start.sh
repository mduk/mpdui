#!/bin/bash
erl \
	-pa deps/*/ebin ebin \
	-s mpdui
