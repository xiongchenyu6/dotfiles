#!/usr/bin/env python
# -*- coding: utf-8 -*-
import netrc


"""
Set a path for your netrc file or ~/.netrc to default path
"""
get = netrc.netrc("/Users/fruittec/.netrc")

"""
Get login, account and password of netrc file
"""
def getPassword(userName) :
    return get.authenticators(userName)[2]
