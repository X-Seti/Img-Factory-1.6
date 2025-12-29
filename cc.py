echo "🧹 Cleaning Python cache files..."
find . -type d -name "__pycache__" -exec rm -rf {} +
echo "Creating new dir tree"
tree  > tree.git
echo "✅ Done!"
