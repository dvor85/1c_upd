# -*- coding: utf-8 -*-
# from __future__ import unicode_literals

import logging
import sys
import os
from logging.handlers import RotatingFileHandler as RFHandler

_log_format = f"%(asctime)-20s %(levelname)7s: [%(module)s->%(funcName)s]: %(message)s"


# def get_file_handler(logfile):
# file_handler = logging.FileHandler(logfile)
# # file_handler.setLevel(logging.WARNING)
#
# file_handler.setFormatter(logging.Formatter(_log_format))
# return file_handler
#
#
# def get_stream_handler():
# stream_handler = logging.StreamHandler()
# # stream_handler.setLevel(logging.INFO)
# stream_handler.setFormatter(logging.Formatter(_log_format))
# return stream_handler
#
#
# def get_logger(name, logfile=None, level=logging.INFO):
# logger = logging.getLogger(name)
# logger.setLevel(level)
# if logfile is not None:
# os.makedirs(os.path.dirname(logfile), exist_ok=True)
# logger.addHandler(get_file_handler(logfile))
# logger.addHandler(get_stream_handler())
# return logger


class Logger(logging.Logger):

    def __init__(self, name, level=logging.NOTSET):
        logging.Logger.__init__(self, name, level=level)

        stream_format = logging.Formatter(_log_format)
        stream_handler = logging.StreamHandler(stream=sys.stdout)
        stream_handler.setFormatter(stream_format)
        self.addHandler(stream_handler)

        self.close_handlers()

    def close_handlers(self):
        """
        It is need for rotating without errors in windows
        """
        if sys.platform.startswith('win'):
            for h in self.handlers:
                h.close()


class MyHandler(logging.Handler):
    terminator = '\n'

    def __init__(self, callback):
        logging.Handler.__init__(self, level=logging.INFO)
        self.callback = callback
        self.setFormatter(logging.Formatter(_log_format))

    def emit(self, record):
        self.callback(self.format(record) + self.terminator)


def get_logger(name, logfile=None, level=logging.NOTSET, callback=None):
    """
    :name The name of the logger to retrieve
    :return: the logger with the specified name
    """
    logging.setLoggerClass(Logger)

    log = logging.getLogger(name)
    log.setLevel(level)
    if logfile is not None:
        os.makedirs(os.path.dirname(logfile), exist_ok=True)
        rfh = RFHandler(filename=logfile, maxBytes=1024 * 1024, backupCount=2)
        rfh.setFormatter(logging.Formatter(_log_format))
        log.addHandler(rfh)

    if callback is not None:
        mh = MyHandler(callback)
        log.addHandler(mh)

    return log
