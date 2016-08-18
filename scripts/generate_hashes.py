import random, string

def random_md5like_hash(length):
    available_chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTVWXYZ0123456789<>.,/?\|}{[]()-_=+*&^%$#@!~'
    return ''.join(random.choice(available_chars) for dummy in xrange(length))

if __name__ == '__main__':
    for i in range(10):
        print(random_md5like_hash(26))
