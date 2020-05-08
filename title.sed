#!/bin/sed -f

1 {
	h
	i <head><title>
	s/^##*  *//
	p
	i </title></head>
	g
}
