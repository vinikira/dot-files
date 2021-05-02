#!/usr/bin/env bash

DATE=$(date +%Y-%m-%d-%H%M%S)
GDRIVEUSER="google-drive://vinicius.simoes95@gmail.com"
BACKUP_DIR="$GDRIVEUSER/0AHkduRDdYicdUk9PVA/1M_5mB8h9uiqU4alVuor2MbxoQju8AFeo"
SOURCE="$HOME/Sync"
TMP_DIR="/tmp/vsimoes-backups"
BACKUP_FILE_NAME="backup-$DATE.tar.gz"

# make backup and move to tmp dir
mkdir -p $TMP_DIR
tar -cvzpf $TMP_DIR/$BACKUP_FILE_NAME $SOURCE

# mount Google Drive
gio mount $GDRIVEUSER

# copy file to backup dir
gio copy $TMP_DIR/$BACKUP_FILE_NAME $BACKUP_DIR
