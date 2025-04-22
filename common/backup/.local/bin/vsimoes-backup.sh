#!/usr/bin/env bash

RCLONE_REMOTE="gdrive"
BACKUP_DIR="$RCLONE_REMOTE:Backups"
SOURCE="$HOME/Syncthing/Sync"
RESTIC_REPO="$HOME/Backups"
BACKUP_FILE="/tmp/vsimoes-backup.tar.gz"

# make backup
restic -r $RESTIC_REPO --verbose --password-command='pass show restic' backup $SOURCE

# compress repo
tar -cvzpf $BACKUP_FILE $RESTIC_REPO

# copy file to backup dir
rclone copy $BACKUP_FILE $BACKUP_DIR
