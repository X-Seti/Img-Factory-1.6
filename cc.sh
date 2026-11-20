echo " Cleaning Python cache files..."
find . -type d -name "__pycache__" -exec rm -rf {} +
echo "Creating new dir tree"
tree  > tree.git
rm -f .git/*.lock .git/*.lock.old* .git/objects/*/tmp_obj_*
echo " Done!"
