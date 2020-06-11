# My Scripts

Generating a new SSH key:
```shell
mkdir -p ~/.ssh
ssh-keygen -t rsa -b 4096 -C "your_email@example.com" -f ~/.ssh/id_rsa_github
echo -e "Host github.com\n  IdentityFile ~/.ssh/id_rsa_github" >> ~/.ssh/config
```

Add the public key to [GitHub](https://github.com/settings/keys):
```shell
cat ~/.ssh/id_rsa_github.pub
```

Clone and install:
```shell
git clone git@github.com:fshaked/scripts.git ~/scripts
make -C ~/scripts install
```
