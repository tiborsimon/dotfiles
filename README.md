# Dotfiles time

This repo contains my dotfile management system that I use to take care of my
OS X and Linux configurations. This project is partially based on
[Zach Holman's dotfiles](https://github.com/holman/dotfiles) project.

Put your binary files and scripts to the `~/.dotfiles/bin` folder, and put your configuration (dot)files to separated folders without the leading dot and the extension of `.symlink`. The `deploy` script will _symlink_ all files with the extension fo `.symlink` to your home folder with a leading dot and without the extension.

# I have a virgin system and I want my config with ease!

This is __exactly__ what I wanted to achieve with this project.

Bootstrap your dotfiles by running the following command __on__ your __virgin system__:

```
sh <(curl -fsSL tiborsimon.io/dotfiles)
```
_You can use your own domain and a simple redirection to your own bootstrap script as well._

This will download the latest _bootstrap script_ (domain redirect to the latest raw github listing of your bootstrap script) and it will clone the repo to your machine and run the actual deployment script.

#### You can clone via https or via ssh as well

The __bootstrap script__ will take care of cloning your __dotfiles__ repo. You only need to decide if you want to use __https__ or __ssh__ protocol for the clone.

## Cloning through HTTPS

This method is the faster one. It clones the repo instantly. But, later if you want to push to the repo, you will have to provide your __GitHub credentials__ (username, password) at every later push. You can avoid this if you use a credential manager software, or use the built in one, and clone the repo through __SSH__.

## Cloning through SSH

If you want to clone your repo through __SSH__ protocol, you will need to have a registered __SSH KEY__ on your machine. The __bootstrap__ script will take care of this:

It will..

- create a new __SSH KEY__ with your email address
- upload your __SSH KEY__ to GitHub

Every step is optional if you have an existing __SSH KEY__.

Both methods will take care of the possible errors. You only need to answer to the asked questions during deployment.

# I have already cloned my dotfiles repo and I want to deploy the pulled updates!

Good. Then go to your `~/.dotfiles` folder and run the  `./deploy` script to do that.

# Example bootstrapping printouts

- __HTTPS__
- __SSH__ with existing __SSH KEY__
- __SSH__ on __virgin system__


### Bootstrapping via HTTPS

```
-bash-4.2$ bash <(curl -fsSL tiborsimon.io/dotfiles)

---------------------------------------------------------------------------------
      T I B O R   S I M O N ' S   D O T F I L E   C O N F I G U R A T I O N
---------------------------------------------------------------------------------

  [ .. ] Installing dotfiles system..
  [ ?? ] Clone repo via [h]ttp or via [g]it (ssh key required)?
  [ OK ] Repo will be cloned via http protocol.
  [ .. ] Cloning dotfiles repository..
Cloning into '/home/vagrant/.dotfiles'...
remote: Counting objects: 32, done.
remote: Compressing objects: 100% (23/23), done.
remote: Total 32 (delta 0), reused 20 (delta 0), pack-reused 0
Unpacking objects: 100% (32/32), done.
  [ OK ] Dotfiles repository cloned.
  [ .. ] Running deploy script..
  [ .. ] Installing Linux related config
  [ .. ] Installing dotfiles..
  [ OK ] linked /home/vagrant/.dotfiles/tmux/tmux.conf.symlink
  [ OK ] linked /home/vagrant/.dotfiles/git/gitconfig.symlink
  [ OK ] linked /home/vagrant/.dotfiles/zsh/zshrc.symlink
  [ OK ] linked /home/vagrant/.dotfiles/vim/vimrc.symlink
  [ OK ] linked /home/vagrant/.dotfiles/bash/bash_profile.symlink
  [ OK ] linked /home/vagrant/.dotfiles/bash/bashrc.symlink
  [ ?? ] Do you want to install packages via yum?  [y]es, [s]kip
  [ OK ] Skipped..
  [ .. ] Installing VimPlug..
  [ OK ] VimPlug installed!
  [ OK ] Dotfiles has been deployed.
  [ OK ] Bootstrap script has finished!
```

### Bootstrapping via SSH with existing SSH KEY

```
-bash-4.2$ bash <(curl -fsSL tiborsimon.io/dotfiles)

---------------------------------------------------------------------------------
      T I B O R   S I M O N ' S   D O T F I L E   C O N F I G U R A T I O N
---------------------------------------------------------------------------------

  [ .. ] Installing dotfiles system..
  [ ?? ] /home/vagrant/.dotfiles folder already exist. [D]elete, [a]bort
  [ OK ] Old /home/vagrant/.dotfiles folder deleted.
  [ ?? ] Clone repo via [h]ttp or via [g]it (ssh key required)?
  [ OK ] Repo will be cloned via git protocol.
  [ .. ] Looking for SSH key..
  [ ?? ] SSH key is detected. Do you want to create a new one? [C]reate, [k]eep
  [ OK ] SSH key generation skipped. Existing key will be used.
  [ .. ] Adding SSH key to ssh-agent..
Enter passphrase for /home/vagrant/.ssh/id_rsa:
Identity added: /home/vagrant/.ssh/id_rsa (/home/vagrant/.ssh/id_rsa)
  [ OK ] SSH key added to ssh-agent.
  [ ?? ] Do you want to add the SSH key to your GitHub account? [Y]es [s]kip
  [ .. ] Skipped uploading SSH key to GitHub.
  [ .. ] Cloning dotfiles repository..
Cloning into '/home/vagrant/.dotfiles'...
remote: Counting objects: 32, done.
remote: Compressing objects: 100% (23/23), done.
remote: Total 32 (delta 0), reused 20 (delta 0), pack-reused 0
Receiving objects: 100% (32/32), 12.55 KiB | 6.00 KiB/s, done.
  [ OK ] Dotfiles repository cloned.
  [ .. ] Running deploy script..
  [ .. ] Installing Linux related config
  [ .. ] Installing dotfiles..
  [ OK ] linked /home/vagrant/.dotfiles/tmux/tmux.conf.symlink
  [ OK ] linked /home/vagrant/.dotfiles/git/gitconfig.symlink
  [ OK ] linked /home/vagrant/.dotfiles/zsh/zshrc.symlink
  [ OK ] linked /home/vagrant/.dotfiles/vim/vimrc.symlink
  [ OK ] linked /home/vagrant/.dotfiles/bash/bash_profile.symlink
  [ OK ] linked /home/vagrant/.dotfiles/bash/bashrc.symlink
  [ ?? ] Do you want to install packages via yum?  [y]es, [s]kip
  [ OK ] Skipped..
  [ .. ] Installing VimPlug..
  [ OK ] VimPlug installed!
  [ OK ] Dotfiles has been deployed.
  [ OK ] Bootstrap script has finished!

```

### Botstrapping via SSH on a virgin machine

```
-bash-4.2$ bash <(curl -fsSL tiborsimon.io/dotfiles)

---------------------------------------------------------------------------------
      T I B O R   S I M O N ' S   D O T F I L E   C O N F I G U R A T I O N
---------------------------------------------------------------------------------

  [ .. ] Installing dotfiles system..
  [ ?? ] Clone repo via [h]ttp or via [g]it (ssh key required)?
  [ OK ] Repo will be cloned via git protocol.
  [ .. ] Looking for SSH key..
  [ ?? ] SSH key (/home/vagrant/.ssh/id_rsa) cannot be found. Generating a new one..
  [ ?? ] What is your email address?
your@domain.com
Generating public/private rsa key pair.
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in /home/vagrant/.ssh/id_rsa.
Your public key has been saved in /home/vagrant/.ssh/id_rsa.pub.
The key fingerprint is:
a1:f4:a6:4a:42:b4:b2:47:4c:4a:44:3d:f4:44:d5:29 your@domain.com
The key's randomart image is:
+--[ RSA 4096]----+
|                 |
|                 |
|        .    F   |
|     . + .       |
|    . = S        |
|   .   I o .     |
| oo     E =      |
|oo+o== o =       |
|.+o=+.o.o        |
+-----------------+
  [ OK ] SSH key generated.
  [ .. ] Adding SSH key to ssh-agent..
Enter passphrase for /home/vagrant/.ssh/id_rsa:
Identity added: /home/vagrant/.ssh/id_rsa (/home/vagrant/.ssh/id_rsa)
  [ OK ] SSH key added to ssh-agent.
  [ ?? ] Do you want to add the SSH key to your GitHub account? [Y]es [s]kip
  [ .. ] Uploading SSH key to tiborsimon's GitHub account..
  [ ?? ] How do you want to name the key?
my-new-ssh-key
Enter host password for user 'tiborsimon':
  [ OK ] Key (my-new-ssh-key) has been added successfully.
  [ .. ] Cloning dotfiles repository..
Cloning into '/home/vagrant/.dotfiles'...
remote: Counting objects: 32, done.
remote: Compressing objects: 100% (23/23), done.
remote: Total 32 (delta 0), reused 18 (delta 0), pack-reused 0
Receiving objects: 100% (32/32), 14.25 KiB | 7.00 KiB/s, done.
  [ OK ] Dotfiles repository cloned.
  [ .. ] Running deploy script..
  [ .. ] Installing Linux related config
  [ .. ] Installing dotfiles..
  [ OK ] linked /home/vagrant/.dotfiles/tmux/tmux.conf.symlink
  [ OK ] linked /home/vagrant/.dotfiles/git/gitconfig.symlink
  [ OK ] linked /home/vagrant/.dotfiles/zsh/zshrc.symlink
  [ OK ] linked /home/vagrant/.dotfiles/vim/vimrc.symlink to /home/vagrant/.vimrc
  [ OK ] linked /home/vagrant/.dotfiles/bash/bash_profile.symlink to /home/vagrant/.bash_profile
  [ OK ] linked /home/vagrant/.dotfiles/bash/bashrc.symlink to /home/vagrant/.bashrc
  [ ?? ] Do you want to install packages via yum?  [y]es, [s]kip
  [ .. ] Executing yum file..
  [ .. ] Installing epel-release..
  [ .. ] Installing git..
  [ .. ] Installing vim..
  [ .. ] Installing tmux..
  [ .. ] Installing mc..
  [ .. ] Installing nc..
  [ OK ] All apps are installed.
  [ .. ] Installing VimPlug..
  [ OK ] VimPlug installed!
  [ OK ] Dotfiles has been deployed.
  [ OK ] Bootstrap script has finished!

```
## License

This project is under the __MIT license__. 
Copyright (c) 2016 Tibor Simon

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


```
 __      __ _____  __  __    ____             _____  _    _    _______  __  __  _    _ __   __      
 \ \    / /|_   _||  \/  |  |  _ \    /\     / ____|| |  | |  |__   __||  \/  || |  | |\ \ / /      
  \ \  / /   | |  | \  / |  | |_) |  /  \   | (___  | |__| |     | |   | \  / || |  | | \ V /       
   \ \/ /    | |  | |\/| |  |  _ <  / /\ \   \___ \ |  __  |     | |   | |\/| || |  | |  > <        
    \  /    _| |_ | |  | |  | |_) |/ ____ \  ____) || |  | |     | |   | |  | || |__| | / . \       
     \/    |_____||_|  |_|  |____//_/    \_\|_____/ |_|  |_|     |_|   |_|  |_| \____/ /_/ \_\      
   _____  _____  _______    _    _   ____   __  __  ______  ____   _____   ______ __          __    
  / ____||_   _||__   __|  | |  | | / __ \ |  \/  ||  ____||  _ \ |  __ \ |  ____|\ \        / /    
 | |  __   | |     | |     | |__| || |  | || \  / || |__   | |_) || |__) || |__    \ \  /\  / /     
 | | |_ |  | |     | |     |  __  || |  | || |\/| ||  __|  |  _ < |  _  / |  __|    \ \/  \/ /      
 | |__| | _| |_    | |     | |  | || |__| || |  | || |____ | |_) || | \ \ | |____    \  /\  /       
  \_____||_____|   |_|     |_|  |_| \____/ |_|  |_||______||____/ |_|  \_\|______|    \/  \/        
  _       _____  _   _  _    _ __   __             _   _  _____      ____    _____ __   __          
 | |     |_   _|| \ | || |  | |\ \ / /      /\    | \ | ||  __ \    / __ \  / ____|\ \ / /          
 | |       | |  |  \| || |  | | \ V /      /  \   |  \| || |  | |  | |  | || (___   \ V /           
 | |       | |  | . ` || |  | |  > <      / /\ \  | . ` || |  | |  | |  | | \___ \   > <            
 | |____  _| |_ | |\  || |__| | / . \    / ____ \ | |\  || |__| |  | |__| | ____) | / . \           
 |______||_____||_| \_| \____/ /_/ \_\  /_/    \_\|_| \_||_____/    \____/ |_____/ /_/ \_\          
  _    _  _____   _        ____    ____    ____  _______  _____  _______  _____             _____   
 | |  | ||  __ \ | |      |  _ \  / __ \  / __ \|__   __|/ ____||__   __||  __ \     /\    |  __ \  
 | |  | || |__) || |      | |_) || |  | || |  | |  | |  | (___     | |   | |__) |   /  \   | |__) | 
 | |  | ||  _  / | |      |  _ < | |  | || |  | |  | |   \___ \    | |   |  _  /   / /\ \  |  ___/  
 | |__| || | \ \ | |____  | |_) || |__| || |__| |  | |   ____) |   | |   | | \ \  / ____ \ | |      
  \____/ |_|  \_\|______| |____/  \____/  \____/   |_|  |_____/    |_|   |_|  \_\/_/    \_\|_|      
 __      __ _____  __  __  _____    _____                                                           
 \ \    / /|_   _||  \/  ||  __ \  / ____|                                                          
  \ \  / /   | |  | \  / || |__) || |                                                               
   \ \/ /    | |  | |\/| ||  _  / | |                                                               
    \  /    _| |_ | |  | || | \ \ | |____                                                           
     \/    |_____||_|  |_||_|  \_\ \_____|
```

