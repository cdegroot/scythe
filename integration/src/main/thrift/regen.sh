#!/bin/bash

thrift -v -out ../java --gen java:java5,hashcode *.thrift
