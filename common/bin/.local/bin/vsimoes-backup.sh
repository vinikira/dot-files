#!/usr/bin/env bash

GDRIVEUSER="google-drive://vinicius.simoes95@gmail.com"
BACKUP_DIR="$GDRIVEUSER/0AHkduRDdYicdUk9PVA/1iFgetgnJU2KBiLE1c_3Z_48q4J558u1K"
SOURCE="$HOME/Sync"
RESTIC_REPO="$HOME/Backups"
BACKUP_FILE="/tmp/vsimoes-backup.tar.gz"

# make backup
restic -r $RESTIC_REPO --verbose --password-command='pass show restic' backup $SOURCE

# compress repo
tar -cvzpf $BACKUP_FILE $RESTIC_REPO

# mount Google Drive
gio mount $GDRIVEUSER

# copy file to backup dir
gio copy $BACKUP_FILE $BACKUP_DIR
