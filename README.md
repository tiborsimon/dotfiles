# Dotfiles time

This repo contains my dotfile management system that I use to take care of my
OS X and Linux configurations. This project is partially based on
[Zach Holman's dotfiles](https://github.com/holman/dotfiles) project.

### Don't you have your config yet?
# Bootstrap it via an URL!

..by running the following command __on__ your __virgin system__:

```
sh <(curl -fsSL tiborsimon.io/dotfiles)
```

This will download the latest _bootstrap script_ (domain redirect to the latest raw github listing of the bootstrap script), and it will clone the repo to your machine, and run the actual deployment script.

You can use your own domain, and a simple redirection to your own __bootstrap__ script's raw listing.

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

# I have already cloned my dotfiles repo, and I want to deploy pulled updates

Good, you can run the `~/.dotfiles/deploy` script from the `.dotfiles` folder to do that.


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

