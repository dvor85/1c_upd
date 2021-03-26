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


class Config(UserDict):
    def __init__(self, confile):
        UserDict.__init__(self)
        self.confile = confile
        self.read()

    def read(self):
        self.data = configparser.ConfigParser()
        self.data.read(self.confile)

    def write(self, config):
        with open(self.confile) as cf:
            self.data.write(cf)
