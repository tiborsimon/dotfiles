#!/usr/bin/env python3


# =============================================================================
#                         B U S I N E S S   L O G I C
# =============================================================================

import os
import random
import struct
import zipfile
import argparse
import shutil

from Crypto.Cipher import AES


class Encryptor:
    def __init__(target):
        self.target = target

    def encrypt(self):
        if not out_filename:
            out_filename = in_filename + '.enc'

        iv = ''.join(chr(random.randint(0, 0xFF)) for i in range(16))
        encryptor = AES.new(key, AES.MODE_CBC, iv)
        filesize = os.path.getsize(in_filename)

        with open(in_filename, 'rb') as infile:
            with open(out_filename, 'wb') as outfile:
                outfile.write(struct.pack('<Q', filesize))
                outfile.write(iv)

                while True:
                    chunk = infile.read(chunksize)
                    if len(chunk) == 0:
                        break
                    elif len(chunk) % 16 != 0:
                        chunk += ' ' * (16 - len(chunk) % 16)

                    outfile.write(encryptor.encrypt(chunk))

    def decrypt(self):
        if not out_filename:
            out_filename = os.path.splitext(in_filename)[0]

        with open(in_filename, 'rb') as infile:
            origsize = struct.unpack('<Q', infile.read(struct.calcsize('Q')))[0]
            iv = infile.read(16)
            decryptor = AES.new(key, AES.MODE_CBC, iv)

            with open(out_filename, 'wb') as outfile:
                while True:
                    chunk = infile.read(chunksize)
                    if len(chunk) == 0:
                        break
                    outfile.write(decryptor.decrypt(chunk))

                outfile.truncate(origsize)

class Zipper:
    def __init__(self, target):
        self.target = target

    def zip(self):
        with zipfile.ZipFile(path, 'w', zipfile.ZIP_BZIP2) as zipf:
            for root, dirs, files in os.walk(path):
                for file in files:
                    zipf.write(os.path.join(root, file))

    def unzip(self):
        with zipfile.ZipFile(path, 'r', zipfile.ZIP_BZIP2) as zipf:
            zipf.extractall()


class TargetSelector:
    def __init__(self, target):
        pass


# =============================================================================
#                            T E S T   S U I T E
# =============================================================================

import unittest




# =============================================================================
#                       M A I N   E N T R Y   P O I N T
# =============================================================================

def main():
    parser = argparse.ArgumentParser(description='Send and receive over TCP')
    parser.add_argument('target', nargs='?', help='target file to be encrypted or decrypted')
    parser.add_argument('--test', action='store_true', help='runs the test suite')
    args = parser.parse_args()


if __name__ == '__main__':
    main()

