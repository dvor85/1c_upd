'''
Created on 26 мар. 2021 г.

@author: demon
'''
import configparser
import os
from collections import UserDict

Version = '2.2.5l'
SectionMain = 'Main'
KeyExecutable = 'Executable'
SectionBase = 'Base'
KeyPath = 'Path'
KeyBakcupPath = 'BakcupPath'
KeyBackupCount = 'BakcupCount'
KeyUser = 'User'
KeyPass = 'Password'
KeyPageSize = 'PageSize'
KeyLogFile = 'LogFile'
SectionUpdates = 'Updates'
SectionMacro = 'Macro'


class Config(configparser.ConfigParser):
    def __init__(self, confile):
        configparser.ConfigParser.__init__(self)
        self.confile = confile
        self.read(self.confile)

    def write(self):
        with open(self.confile, "w") as fp:
            configparser.ConfigParser.write(self, fp)
