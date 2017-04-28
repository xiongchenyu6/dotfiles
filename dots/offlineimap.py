import os

def getnetrc(**match):
    '''A dumb filter for ~/.netrc. It understands oneline entries,
    comments and default entry. No macros or multiline entries.

    Return first matching dict or None.'''
    authinfo = '~/.netrc'

    for li in open(os.path.expanduser(authinfo)).readlines():
        li = li.partition('#')[0].split() # current line in list
        if not li:
            continue
        if li[0] == 'default':
            default = dict(zip(li[1:-1:2], li[2::2]))
            continue
        elif li[0] == 'macdef':
            continue

        li = dict(zip(li[:-1:2], li[1::2])) # current line in dict

        if contains(li, match):
            return li
    return li

def contains(d, m):
    '''Return True if d contains all items of m.'''
    for k in m:
        if not k in d or d[k] != m[k]:
            return False
    return True
# debug
if __name__ == "__main__":
    print getnetrc(machine='imap.gmail.com', login='xiongchenyu6@gmail.com')
