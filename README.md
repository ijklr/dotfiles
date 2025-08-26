# Dotfiles

My personal dotfiles, managed with [GNU Stow](https://www.gnu.org/software/stow/).

This repo keeps configs under version control and symlinks them into `~` (or `$XDG_CONFIG_HOME`) so programs find them where expected.

---

## 📦 Requirements

- Git
- [GNU Stow](https://www.gnu.org/software/stow/) (`sudo apt install stow`, `sudo pacman -S stow`, `brew install stow`, etc.)

---

## 🔧 Setup

Clone into `~/dotfiles`:

```bash
git clone git@github.com:ijklr/dotfiles.git ~/dotfiles
cd ~/dotfiles

run:
stow emac