#this belongs in components/ img_project_manager.py - version 1
# X-Seti - July06 2025 - Img Factory 1.5
# Credit MexUK 2007 Img Factory 1.2

#!/usr/bin/env python3
"""
IMG Project Manager - Organize IMG files, exports, and project resources
Central project management for IMG Factory 1.5
"""

import os
import json
import shutil
import time
from pathlib import Path
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
from datetime import datetime

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QFormLayout, QGridLayout,
    QLabel, QPushButton, QLineEdit, QTextEdit, QTreeWidget, QTreeWidgetItem,
    QGroupBox, QFileDialog, QMessageBox, QTabWidget, QWidget,
    QListWidget, QListWidgetItem, QComboBox, QCheckBox, QProgressBar
)
from PyQt6.QtCore import Qt, pyqtSignal, QTimer
from PyQt6.QtGui import QFont, QIcon


@dataclass
class IMGProject:
    """IMG Factory project structure"""
    name: str
    path: Path
    target_game: str = "gtasa"  # Target game for this project
    description: str = ""
    created_date: str = field(default_factory=lambda: datetime.now().isoformat())
    last_modified: str = field(default_factory=lambda: datetime.now().isoformat())
    version: str = "1.0"
    
    # Project statistics
    img_files_count: int = 0
    exported_files_count: int = 0
    total_size_mb: float = 0.0
    
    # Project settings
    auto_organize: bool = True
    backup_enabled: bool = True
    compression_default: bool = False
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert project to dictionary"""
        return {
            'name': self.name,
            'path': str(self.path),
            'target_game': self.target_game,
            'description': self.description,
            'created_date': self.created_date,
            'last_modified': self.last_modified,
            'version': self.version,
            'img_files_count': self.img_files_count,
            'exported_files_count': self.exported_files_count,
            'total_size_mb': self.total_size_mb,
            'auto_organize': self.auto_organize,
            'backup_enabled': self.backup_enabled,
            'compression_default': self.compression_default
        }
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'IMGProject':
        """Create project from dictionary"""
        return cls(
            name=data['name'],
            path=Path(data['path']),
            target_game=data.get('target_game', 'gtasa'),
            description=data.get('description', ''),
            created_date=data.get('created_date', datetime.now().isoformat()),
            last_modified=data.get('last_modified', datetime.now().isoformat()),
            version=data.get('version', '1.0'),
            img_files_count=data.get('img_files_count', 0),
            exported_files_count=data.get('exported_files_count', 0),
            total_size_mb=data.get('total_size_mb', 0.0),
            auto_organize=data.get('auto_organize', True),
            backup_enabled=data.get('backup_enabled', True),
            compression_default=data.get('compression_default', False)
        )


class IMGProjectManager:
    """Manages IMG Factory projects"""
    
    def __init__(self, base_projects_path: Optional[str] = None):
        if base_projects_path is None:
            # Default to user's Documents/IMG Factory Projects
            self.base_path = Path.home() / "Documents" / "IMG Factory Projects"
        else:
            self.base_path = Path(base_projects_path)
        
        self.base_path.mkdir(parents=True, exist_ok=True)
        self.projects_file = self.base_path / "projects.json"
        
        self.projects: List[IMGProject] = []
        self.current_project: Optional[IMGProject] = None
        
        self._load_projects()
    
    def create_project(self, name: str, target_game: str, description: str = "") -> Optional[IMGProject]:
        """Create new IMG project with game-specific directory structure"""
        try:
            # Sanitize project name for filesystem
            safe_name = "".join(c for c in name if c.isalnum() or c in (' ', '-', '_')).strip()
            if not safe_name:
                safe_name = f"Project_{int(time.time())}"
            
            project_path = self.base_path / safe_name
            
            # Check if project already exists
            if project_path.exists():
                raise FileExistsError(f"Project '{safe_name}' already exists")
            
            # Create project directory structure based on target game
            self._create_project_structure(project_path, target_game)
            
            # Create project object
            project = IMGProject(
                name=name,
                path=project_path,
                target_game=target_game,
                description=description
            )
            
            # Save project info
            self._save_project_info(project)
            
            # Add to projects list
            self.projects.append(project)
            self._save_projects()
            
            print(f"✅ Created {target_game.upper()} project: {name} at {project_path}")
            return project
            
        except Exception as e:
            print(f"❌ Error creating project: {e}")
            return None
    
    def _create_project_structure(self, project_path: Path, target_game: str):
        """Create game-specific project directory structure"""
        
        # Base directories for all projects
        base_directories = [
            "created_imgs",      # Newly created IMG files
            "imported_imgs",     # Imported/opened IMG files  
            "exported_files",    # Exported file collections
            "extracted_files",   # Individual extracted files
            "templates",         # Custom templates
            "backups",          # Automatic backups
            "temp",             # Temporary working files
            
            # File type organization
            "tex_files",         # Texture files (.txd, .tex)
            "col_files",         # Collision files (.col)
            "dff_files",         # Model files (.dff)
            "misc_files",        # Other file types
        ]
        
        # Game-specific configurations
        game_configs = {
            'gta3': {
                'folders': ['ifp_files'],  # GTA3 uses IFP animations
                'subfolders': [
                    'gta3/vehicles', 'gta3/peds', 'gta3/weapons', 'gta3/buildings',
                    'gta3/textures', 'gta3/sounds'
                ]
            },
            'gtavc': {
                'folders': ['ifp_files'],  # Vice City uses IFP animations
                'subfolders': [
                    'gtavc/vehicles', 'gtavc/peds', 'gtavc/weapons', 'gtavc/buildings',
                    'gtavc/textures', 'gtavc/sounds', 'gtavc/cutscenes'
                ]
            },
            'gtasa': {
                'folders': ['ifp_files', 'wav_files'],  # SA has both IFP and WAV
                'subfolders': [
                    'gtasa/vehicles', 'gtasa/peds', 'gtasa/weapons', 'gtasa/buildings',
                    'gtasa/textures', 'gtasa/sounds', 'gtasa/interiors', 'gtasa/effects'
                ]
            },
            'bully': {
                'folders': ['wav_files'],  # Bully primarily uses WAV files
                'subfolders': [
                    'bully/characters', 'bully/world', 'bully/textures', 
                    'bully/sounds', 'bully/effects'
                ]
            },
            'manhunt': {
                'folders': [],
                'subfolders': [
                    'manhunt/characters', 'manhunt/world', 'manhunt/textures', 'manhunt/sounds'
                ]
            },
            'custom': {
                'folders': ['ifp_files', 'wav_files'],  # Include all for custom projects
                'subfolders': [
                    'custom/vehicles', 'custom/characters', 'custom/world', 
                    'custom/textures', 'custom/sounds', 'custom/other'
                ]
            },
            'sol': {
                'folders': ['ifp_files', 'wav_files', 'dat_files', 'scm_files', 'bmp_files', 'asi_files'],  # SOL uses all file types + ASI mods
                'subfolders': [
                    # Multi-city organization like SOL
                    'sol/liberty_city', 'sol/vice_city', 'sol/san_andreas',
                    'sol/mainland', 'sol/islands', 'sol/interiors',
                    # Vehicle organization
                    'sol/vehicles/gta3_style', 'sol/vehicles/vc_style', 'sol/vehicles/sa_style',
                    # Weapon organization  
                    'sol/weapons/classic', 'sol/weapons/modern', 'sol/weapons/custom',
                    # Map data
                    'sol/maps/ipl_files', 'sol/maps/ide_files', 'sol/maps/col_files',
                    'sol/maps/zones', 'sol/maps/paths',
                    # Textures by location and resolution
                    'sol/textures/loadscreens', 'sol/textures/hud', 'sol/textures/generic',
                    'sol/textures/hd_2048', 'sol/textures/radar',
                    # Audio organization
                    'sol/audio/sfx', 'sol/audio/music', 'sol/audio/radio',
                    # Scripts and data
                    'sol/scripts', 'sol/data_files', 'sol/configs',
                    # ASI mods and plugins
                    'sol/asi_mods', 'sol/limit_adjusters', 'sol/core_mods',
                    # High-res assets
                    'sol/hd_bitmaps', 'sol/water_masks', 'sol/64bit_imgs'
                ]
            }
        }
        
        # Get configuration for target game
        config = game_configs.get(target_game, game_configs['custom'])
        
        # Create base directories
        all_directories = base_directories.copy()
        
        # Add game-specific folders
        all_directories.extend(config['folders'])
        
        # Add special directories for certain game types
        if target_game == 'sol':
            # SOL-specific additional directories based on the tree structure
            sol_extras = [
                'cdimages',          # For IMG/DIR files like in sol/CDimages/
                'maps',              # For IPL/IDE files like in sol/maps/
                'data_configs',      # For DAT/CFG files  
                'audio_sfx',         # For WAV/audio files
                'txd_loadscreens',   # For TXD files like in txd/
                'scripts_scm',       # For SCM files
                'zones_paths',       # For zone and path files
                'mvl_mods',          # For MVL vehicle mods
                'weapon_mods',       # For weapon modifications
                'asi_plugins',       # For ASI mod files (like limit_adjuster_gta3vcsa.asi)
                'hd_bitmaps',        # For high-res 2048x2048 bitmaps
                'water_textures',    # For water masks and effects
                'radar_textures',    # For radar/map textures
                'dll_libraries',     # For DLL files (Mss32.dll, core.dll, etc)
                'exe_tools',         # For EXE tools and utilities
                'cleo_scripts',      # For CLEO ASI and scripts
                'mss_audio',         # For MSS audio system files
                'limit_configs'      # For limit adjuster configurations
            ]
            all_directories.extend(sol_extras)
        
        # Add sorted_files with game-specific subfolders
        all_directories.append('sorted_files')
        for subfolder in config['subfolders']:
            all_directories.append(f'sorted_files/{subfolder}')
        
        # Create all directories
        for dir_name in all_directories:
            (project_path / dir_name).mkdir(parents=True, exist_ok=True)
        
        # Create .gitignore for temp files
        gitignore_content = """# IMG Factory temporary files
temp/
*.tmp
*.bak
__pycache__/
*.pyc
"""
        (project_path / ".gitignore").write_text(gitignore_content)
        
        # Create game-specific README
        self._create_game_readme(project_path, target_game, config)
        
    def _create_game_readme(self, project_path: Path, target_game: str, config: Dict):
        """Create game-specific README file"""
        game_names = {
            'gta3': 'Grand Theft Auto III',
            'gtavc': 'Grand Theft Auto: Vice City', 
            'gtasa': 'Grand Theft Auto: San Andreas',
            'bully': 'Bully',
            'manhunt': 'Manhunt',
            'custom': 'Custom/Multi-Game'
        }
        
        game_name = game_names.get(target_game, target_game.upper())
        
        readme_content = f"""# IMG Factory Project - {game_name}
Created: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
Target Game: **{game_name}**

## Directory Structure

### IMG Archives
- `created_imgs/` - New IMG files created for {game_name}
- `imported_imgs/` - {game_name} IMG files imported from game directory

### File Type Organization
- `tex_files/` - Texture files (.txd, .tex)
- `col_files/` - Collision files (.col) 
- `dff_files/` - Model files (.dff)
- `misc_files/` - Other file types"""

        # Add game-specific file types
        if 'ifp_files' in config['folders']:
            readme_content += "\n- `ifp_files/` - Animation files (.ifp)"
        if 'wav_files' in config['folders']:
            readme_content += "\n- `wav_files/` - Audio files (.wav, .ogg)"

        readme_content += f"""

### {game_name} Specific Organization
- `sorted_files/{target_game}/` - {game_name} categorized files"""

        # Add subfolder descriptions
        for subfolder in config['subfolders']:
            folder_name = subfolder.split('/')[-1]
            readme_content += f"\n  - `{folder_name}/` - {folder_name.title()} related files"

        readme_content += """

### Project Management
- `exported_files/` - Bulk exported file collections
- `extracted_files/` - Individual files extracted from IMG archives
- `templates/` - Custom IMG creation templates
- `backups/` - Automatic backups of modified files
- `temp/` - Temporary working files

## Usage
This project was created with IMG Factory 1.5 specifically for working with """ + game_name + """ files.
Files are automatically organized by type and category when imported.
Use the project manager to maintain a clean workflow.
"""
        
        (project_path / "README.md").write_text(readme_content)
    
    def _save_project_info(self, project: IMGProject):
        """Save project information to project directory"""
        project_info_file = project.path / "project.json"
        with open(project_info_file, 'w', encoding='utf-8') as f:
            json.dump(project.to_dict(), f, indent=2, ensure_ascii=False)
    
    def get_project_path(self, project: IMGProject, subfolder: str) -> Path:
        """Get path to project subfolder"""
        return project.path / subfolder
    
    def get_created_imgs_path(self, project: IMGProject) -> Path:
        """Get path for newly created IMG files"""
        return self.get_project_path(project, "created_imgs")
    
    def get_exported_files_path(self, project: IMGProject) -> Path:
        """Get path for exported files"""
        return self.get_project_path(project, "exported_files")
    
    def get_sorted_files_path(self, project: IMGProject, game_type: str = "") -> Path:
        """Get path for sorted files"""
        if game_type:
            return self.get_project_path(project, f"sorted_files/{game_type}")
        return self.get_project_path(project, "sorted_files")
    
    def get_tex_files_path(self, project: IMGProject) -> Path:
        """Get path for texture files (.txd, .tex)"""
        return self.get_project_path(project, "tex_files")
    
    def get_col_files_path(self, project: IMGProject) -> Path:
        """Get path for collision files (.col)"""
        return self.get_project_path(project, "col_files")
    
    def get_dff_files_path(self, project: IMGProject) -> Path:
        """Get path for model files (.dff)"""
        return self.get_project_path(project, "dff_files")
    
    def get_ifp_files_path(self, project: IMGProject) -> Path:
        """Get path for animation files (.ifp)"""
        return self.get_project_path(project, "ifp_files")
    
    def get_wav_files_path(self, project: IMGProject) -> Path:
        """Get path for audio files (.wav, .ogg)"""
        return self.get_project_path(project, "wav_files")
    
    def get_dat_files_path(self, project: IMGProject) -> Path:
        """Get path for data configuration files (.dat, .cfg)"""
        return self.get_project_path(project, "dat_files")
    
    def get_scm_files_path(self, project: IMGProject) -> Path:
        """Get path for script files (.scm)"""
        return self.get_project_path(project, "scm_files")
    
    def get_cdimages_path(self, project: IMGProject) -> Path:
        """Get path for CD images (SOL-style IMG organization)"""
        return self.get_project_path(project, "cdimages")
    
    def get_bmp_files_path(self, project: IMGProject) -> Path:
        """Get path for bitmap files (.bmp) including high-res water masks"""
        return self.get_project_path(project, "bmp_files")
    
    def get_asi_files_path(self, project: IMGProject) -> Path:
        """Get path for ASI modification files"""
        return self.get_project_path(project, "asi_files")
    
    def get_hd_bitmaps_path(self, project: IMGProject) -> Path:
        """Get path for high-resolution bitmaps (2048x2048+)"""
        return self.get_project_path(project, "hd_bitmaps")
    
    def get_dll_libraries_path(self, project: IMGProject) -> Path:
        """Get path for DLL library files"""
        return self.get_project_path(project, "dll_libraries")
    
    def get_asi_plugins_path(self, project: IMGProject) -> Path:
        """Get path for ASI plugin files"""
        return self.get_project_path(project, "asi_plugins")
    
    def detect_img_format(self, file_path: str) -> str:
        """Detect IMG file format (32-bit vs 64-bit, version, etc.)"""
        try:
            with open(file_path, 'rb') as f:
                # Read first 8 bytes to detect format
                header = f.read(8)
                
                if len(header) < 4:
                    return "unknown"
                
                # Check for version signatures
                if header[:4] == b'VER2':
                    # Check file size to estimate if it's 64-bit capable
                    f.seek(0, 2)  # Seek to end
                    file_size = f.tell()
                    
                    if file_size > 2**32:  # > 4GB suggests 64-bit
                        return "ver2_64bit"
                    else:
                        return "ver2_32bit"
                elif header[:4] == b'VER3':
                    return "ver3_64bit"
                else:
                    # Might be version 1 (DIR+IMG)
                    return "ver1_classic"
                    
        except Exception:
            return "unknown"
    
    def detect_texture_resolution(self, file_path: str) -> str:
        """Detect texture resolution for TXD files"""
        try:
            file_size = os.path.getsize(file_path)
            filename = os.path.basename(file_path).lower()
            
            # Heuristic detection based on file size and naming
            if file_size > 50 * 1024 * 1024:  # > 50MB suggests very high res
                return "ultra_hd"
            elif file_size > 10 * 1024 * 1024:  # > 10MB suggests HD
                return "hd_2048"
            elif any(x in filename for x in ['hd', '2048', '1080p', '4k']):
                return "hd_enhanced"
            else:
                return "standard"
                
        except Exception:
            return "unknown"
    
    def organize_file(self, project: IMGProject, file_path: str, file_type: str, game_type: str = "other") -> bool:
        """Organize file into appropriate project directory"""
        try:
            source_path = Path(file_path)
            if not source_path.exists():
                return False
            
            # Determine target directory
            if file_type == "img":
                target_dir = self.get_project_path(project, "imported_imgs")
            elif file_type == "exported":
                target_dir = self.get_exported_files_path(project)
            elif file_type == "sorted":
                target_dir = self.get_sorted_files_path(project, game_type)
            else:
                target_dir = self.get_project_path(project, "temp")
            
            # Create target directory if needed
            target_dir.mkdir(parents=True, exist_ok=True)
            
            # Copy file to target location
            target_path = target_dir / source_path.name
            shutil.copy2(source_path, target_path)
            
            # Update project statistics
            self._update_project_stats(project)
            
            print(f"✅ Organized {source_path.name} → {target_dir}")
            return True
            
        except Exception as e:
            print(f"❌ Error organizing file: {e}")
            return False
    
    def create_backup(self, project: IMGProject, file_path: str) -> bool:
        """Create backup of file in project backups directory"""
        try:
            if not project.backup_enabled:
                return True  # Backups disabled
            
            source_path = Path(file_path)
            if not source_path.exists():
                return False
            
            backup_dir = self.get_backups_path(project)
            backup_dir.mkdir(parents=True, exist_ok=True)
            
            # Create timestamped backup
            timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
            backup_name = f"{source_path.stem}_{timestamp}{source_path.suffix}"
            backup_path = backup_dir / backup_name
            
            shutil.copy2(source_path, backup_path)
            print(f"✅ Backup created: {backup_name}")
            return True
            
        except Exception as e:
            print(f"❌ Error creating backup: {e}")
            return False
    
    def _update_project_stats(self, project: IMGProject):
        """Update project statistics"""
        try:
            img_count = len(list(project.path.glob("**/*.img")))
            exported_count = len(list(self.get_exported_files_path(project).glob("*")))
            
            # Calculate total size
            total_size = 0
            for file_path in project.path.rglob("*"):
                if file_path.is_file():
                    total_size += file_path.stat().st_size
            
            project.img_files_count = img_count
            project.exported_files_count = exported_count
            project.total_size_mb = total_size / (1024 * 1024)
            project.last_modified = datetime.now().isoformat()
            
            # Save updated info
            self._save_project_info(project)
            self._save_projects()
            
        except Exception as e:
            print(f"❌ Error updating project stats: {e}")
    
    def _load_projects(self):
        """Load projects from JSON file"""
        try:
            if self.projects_file.exists():
                with open(self.projects_file, 'r', encoding='utf-8') as f:
                    projects_data = json.load(f)
                
                self.projects = [
                    IMGProject.from_dict(proj_data) 
                    for proj_data in projects_data.get('projects', [])
                ]
                
                print(f"✅ Loaded {len(self.projects)} projects")
            else:
                self.projects = []
                
        except Exception as e:
            print(f"❌ Error loading projects: {e}")
            self.projects = []
    
    def _save_projects(self):
        """Save projects to JSON file"""
        try:
            projects_data = {
                'version': '1.0',
                'last_updated': datetime.now().isoformat(),
                'projects': [proj.to_dict() for proj in self.projects]
            }
            
            with open(self.projects_file, 'w', encoding='utf-8') as f:
                json.dump(projects_data, f, indent=2, ensure_ascii=False)
                
        except Exception as e:
            print(f"❌ Error saving projects: {e}")
    
    def get_all_projects(self) -> List[IMGProject]:
        """Get all available projects"""
        return self.projects.copy()
    
    def get_project_by_name(self, name: str) -> Optional[IMGProject]:
        """Get project by name"""
        for project in self.projects:
            if project.name == name:
                return project
        return None
    
    def set_current_project(self, project: IMGProject):
        """Set current active project"""
        self.current_project = project
        print(f"✅ Active project: {project.name}")
    
    def get_current_project(self) -> Optional[IMGProject]:
        """Get current active project"""
        return self.current_project


class ProjectManagerDialog(QDialog):
    """Dialog for managing IMG projects"""
    
    project_selected = pyqtSignal(object)  # Emits IMGProject
    
    def __init__(self, project_manager: IMGProjectManager, parent=None):
        super().__init__(parent)
        self.project_manager = project_manager
        self.setWindowTitle("IMG Project Manager")
        self.setMinimumSize(700, 500)
        self.setModal(True)
        
        self._setup_ui()
        self._load_projects()
    
    def _setup_ui(self):
        """Setup the user interface"""
        layout = QVBoxLayout(self)
        
        # Title
        title = QLabel("🗂️ IMG Project Manager")
        title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title.setStyleSheet("font-size: 16px; font-weight: bold; color: #2E7D32; margin: 10px;")
        layout.addWidget(title)
        
        # Tab widget
        tab_widget = QTabWidget()
        
        # Projects tab
        projects_tab = QWidget()
        projects_layout = QVBoxLayout(projects_tab)
        
        # Project list
        self.projects_list = QTreeWidget()
        self.projects_list.setHeaderLabels(['Project', 'Location', 'Files', 'Size'])
        self.projects_list.itemDoubleClicked.connect(self._open_project)
        projects_layout.addWidget(self.projects_list)
        
        # Project buttons
        proj_buttons_layout = QHBoxLayout()
        
        self.new_project_btn = QPushButton("📁 New Project")
        self.new_project_btn.clicked.connect(self._new_project)
        proj_buttons_layout.addWidget(self.new_project_btn)
        
        self.open_project_btn = QPushButton("Open Project")
        self.open_project_btn.clicked.connect(self._open_project)
        proj_buttons_layout.addWidget(self.open_project_btn)
        
        self.delete_project_btn = QPushButton("🗑️ Delete Project")
        self.delete_project_btn.clicked.connect(self._delete_project)
        proj_buttons_layout.addWidget(self.delete_project_btn)
        
        proj_buttons_layout.addStretch()
        projects_layout.addLayout(proj_buttons_layout)
        
        tab_widget.addTab(projects_tab, "Projects")
        
        # Settings tab
        settings_tab = QWidget()
        settings_layout = QFormLayout(settings_tab)
        
        # Base path setting
        self.base_path_edit = QLineEdit(str(self.project_manager.base_path))
        base_path_btn = QPushButton("Browse...")
        base_path_btn.clicked.connect(self._browse_base_path)
        
        base_path_layout = QHBoxLayout()
        base_path_layout.addWidget(self.base_path_edit)
        base_path_layout.addWidget(base_path_btn)
        
        settings_layout.addRow("Projects Base Path:", base_path_layout)
        
        # Default settings
        self.auto_organize_check = QCheckBox("Auto-organize files by type")
        self.auto_organize_check.setChecked(True)
        settings_layout.addRow("Organization:", self.auto_organize_check)
        
        self.backup_enabled_check = QCheckBox("Enable automatic backups")
        self.backup_enabled_check.setChecked(True)
        settings_layout.addRow("Backups:", self.backup_enabled_check)
        
        tab_widget.addTab(settings_tab, "Settings")
        
        layout.addWidget(tab_widget)
        
        # Dialog buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()
        
        self.close_btn = QPushButton("Close")
        self.close_btn.clicked.connect(self.accept)
        button_layout.addWidget(self.close_btn)
        
        layout.addLayout(button_layout)
    
    def _load_projects(self):
        """Load projects into the tree widget"""
        self.projects_list.clear()
        
        for project in self.project_manager.get_all_projects():
            # Get game emoji for display
            game_emojis = {
                'gta3': '🚗', 'gtavc': '🌴', 'gtasa': '🏜️', 
                'bully': '🏫', 'manhunt': '🔪', 'sol': '🏙️', 'custom': '⚙️'
            }
            
            game_emoji = game_emojis.get(project.target_game, '❓')
            project_display = f"{game_emoji} {project.name}"
            
            item = QTreeWidgetItem([
                project_display,
                str(project.path),
                f"{project.img_files_count} IMG files",
                f"{project.total_size_mb:.1f} MB"
            ])
            item.setData(0, Qt.ItemDataRole.UserRole, project)
            self.projects_list.addItem(item)
    
    def _new_project(self):
        """Create new project"""
        dialog = NewProjectDialog(self.project_manager, self)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            self._load_projects()
    
    def _open_project(self):
        """Open selected project"""
        current_item = self.projects_list.currentItem()
        if current_item:
            project = current_item.data(0, Qt.ItemDataRole.UserRole)
            self.project_manager.set_current_project(project)
            self.project_selected.emit(project)
            self.accept()
    
    def _delete_project(self):
        """Delete selected project"""
        current_item = self.projects_list.currentItem()
        if not current_item:
            return
        
        project = current_item.data(0, Qt.ItemDataRole.UserRole)
        
        reply = QMessageBox.question(
            self, "Delete Project",
            f"Delete project '{project.name}'?\n\nThis will remove the project directory and all files.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
        )
        
        if reply == QMessageBox.StandardButton.Yes:
            try:
                shutil.rmtree(project.path)
                self.project_manager.projects.remove(project)
                self.project_manager._save_projects()
                self._load_projects()
                QMessageBox.information(self, "Success", "Project deleted successfully!")
            except Exception as e:
                QMessageBox.critical(self, "Error", f"Failed to delete project:\n{str(e)}")
    
    def _browse_base_path(self):
        """Browse for base projects path"""
        path = QFileDialog.getExistingDirectory(self, "Select Projects Base Directory")
        if path:
            self.base_path_edit.setText(path)


class NewProjectDialog(QDialog):
    """Dialog for creating new project"""
    
    def __init__(self, project_manager: IMGProjectManager, parent=None):
        super().__init__(parent)
        self.project_manager = project_manager
        self.setWindowTitle("Create New Project")
        self.setFixedSize(400, 300)
        self.setModal(True)
        
        self._setup_ui()
    
    def _setup_ui(self):
        """Setup the user interface"""
        layout = QVBoxLayout(self)
        
        # Title
        title = QLabel("📁 Create New IMG Project")
        title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title.setStyleSheet("font-size: 14px; font-weight: bold; margin: 10px;")
        layout.addWidget(title)
        
        # Form
        form_layout = QFormLayout()
        
        # Project name
        self.name_edit = QLineEdit()
        self.name_edit.setPlaceholderText("Enter project name...")
        form_layout.addRow("Project Name:", self.name_edit)
        
        # Target game selection
        self.game_combo = QComboBox()
        game_options = [
            ("gta3", "🚗 Grand Theft Auto III"),
            ("gtavc", "🌴 Grand Theft Auto: Vice City"),
            ("gtasa", "🏜️ Grand Theft Auto: San Andreas"),
            ("bully", "🏫 Bully"),
            ("manhunt", "🔪 Manhunt"),
            ("sol", "🏙️ State of Liberty (Multi-GTA)"),
            ("custom", "⚙️ Custom/Multi-Game")
        ]
        
        for code, name in game_options:
            self.game_combo.addItem(name, code)
        
        # Default to GTA SA
        self.game_combo.setCurrentIndex(2)
        form_layout.addRow("Target Game:", self.game_combo)
        
        # Game description
        self.game_desc_label = QLabel()
        self.game_desc_label.setWordWrap(True)
        self.game_desc_label.setStyleSheet("color: #666; font-size: 11px; font-style: italic;")
        form_layout.addRow("", self.game_desc_label)
        
        # Description
        self.desc_edit = QTextEdit()
        self.desc_edit.setMaximumHeight(80)
        self.desc_edit.setPlaceholderText("Optional project description...")
        form_layout.addRow("Description:", self.desc_edit)
        
        # Project location preview
        self.location_label = QLabel()
        self.location_label.setStyleSheet("color: #666; font-style: italic;")
        form_layout.addRow("Location:", self.location_label)
        
        layout.addLayout(form_layout)
        
        # Update location preview when name changes
        self.name_edit.textChanged.connect(self._update_location_preview)
        self.game_combo.currentTextChanged.connect(self._update_game_description)
        
        # Initialize game description
        self._update_game_description()
        
        layout.addStretch()
        
        # Buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()
        
        self.cancel_btn = QPushButton("Cancel")
        self.cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(self.cancel_btn)
        
        self.create_btn = QPushButton("Create Project")
        self.create_btn.clicked.connect(self._create_project)
        self.create_btn.setEnabled(False)
        button_layout.addWidget(self.create_btn)
        
        layout.addLayout(button_layout)
        
        # Enable create button when name is entered
        self.name_edit.textChanged.connect(
            lambda text: self.create_btn.setEnabled(bool(text.strip()))
        )
    
    def _update_game_description(self):
        """Update game description based on selection"""
        game_code = self.game_combo.currentData()
        
        descriptions = {
            'gta3': 'Creates folders for vehicles, peds, weapons, buildings. Includes IFP animation support.',
            'gtavc': 'Vice City layout with vehicles, peds, weapons, buildings, cutscenes. IFP animations.',
            'gtasa': 'Full SA structure: vehicles, peds, weapons, interiors, effects. IFP + WAV support.',
            'bully': 'Bully-specific: characters, world, textures, sounds. WAV audio support.',
            'manhunt': 'Manhunt layout: characters, world, textures, sounds.',
            'sol': 'State of Liberty multi-city structure: LC/VC/SA areas, MVL mods, CDimages, maps, scripts.',
            'custom': 'All file types and flexible organization for multiple games.'
        }
        
        self.game_desc_label.setText(descriptions.get(game_code, ''))
    
    def _update_location_preview(self, name: str):
        """Update location preview"""
        if name.strip():
            safe_name = "".join(c for c in name if c.isalnum() or c in (' ', '-', '_')).strip()
            location = self.project_manager.base_path / safe_name
            self.location_label.setText(str(location))
        else:
            self.location_label.setText("")
    
    def _create_project(self):
        """Create the project"""
        name = self.name_edit.text().strip()
        description = self.desc_edit.toPlainText().strip()
        target_game = self.game_combo.currentData()
        
        if not name:
            QMessageBox.warning(self, "Warning", "Please enter a project name.")
            return
        
        project = self.project_manager.create_project(name, target_game, description)
        if project:
            game_name = self.game_combo.currentText().split(' ', 1)[1]  # Remove emoji
            QMessageBox.information(
                self, "Success", 
                f"Project '{name}' created successfully!\n\n"
                f"Target Game: {game_name}\n"
                f"Location: {project.path}"
            )
            self.accept()
        else:
            QMessageBox.critical(self, "Error", "Failed to create project.")


# Factory functions
def create_project_manager(base_path: Optional[str] = None) -> IMGProjectManager:
    """Create project manager instance"""
    return IMGProjectManager(base_path)


def show_project_manager_dialog(project_manager: IMGProjectManager, parent=None) -> Optional[IMGProject]:
    """Show project manager dialog and return selected project"""
    dialog = ProjectManagerDialog(project_manager, parent)
    selected_project = None
    
    def on_project_selected(project):
        nonlocal selected_project
        selected_project = project
    
    dialog.project_selected.connect(on_project_selected)
    
    if dialog.exec() == QDialog.DialogCode.Accepted:
        return selected_project
    return None