#!/bin/sh

sed '1 {
	h
	i\
<head><title>
	s/^##*  *//
	p
	i\
</title></head>
	g
}'
