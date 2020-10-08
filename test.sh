#!/bin/sh
KMGC=$(stack exec -- command -v kmgc)

FAIL=0
for i in tests/c/*.kmg ; do
  ${KMGC} ${i} >${i}.actual 2>&1
  OK=$?
  if ! (cmp ${i}.actual ${i}.$OK >/dev/null 2>&1) ; then
    echo ${i} failed:
    if [ -f ${i}.$OK ] ; then
      diff -u ${i}.actual ${i}.$OK
    else
      echo "-> no ${i}.$OK, actual follows:"
      cat ${i}.actual
    fi
    FAIL=$((FAIL + 1))
  fi
done

if [ $FAIL -gt 0 ] ; then
  exit 1
fi
