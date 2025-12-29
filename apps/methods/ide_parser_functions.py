#this belongs in methods/ide_parser.py - Version: 1
# X-Seti - July31 2025 - IMG Factory 1.5 - Universal IDE Parser

"""
Universal IDE Parser - Moved from split_img.py
Used by: import_via, export_via, remove_via, split_via
Provides consistent IDE parsing across all "via" operations
"""

import os
from typing import Dict, List, Set, Optional, Tuple

##Methods list -
# integrate_ide_parser
# parse_ide_content
# parse_ide_file

##Classes -
# IDEParser

class IDEParser:
    """Universal parser for IDE files to extract model and texture relationships"""
    
    def __init__(self): #vers 1
        """Initialize IDE parser"""
        self.models = {}  # model_id -> {name, txd, dff, type, section}
        self.textures = {}  # txd_name -> [model_names]
        self.sections = {}  # section_name -> [entries]
        self.file_path = ""
        self.parse_stats = {
            'total_lines': 0,
            'parsed_lines': 0,
            'models_found': 0,
            'sections_found': 0,
            'errors': []
        }
        
    def parse_ide_file(self, ide_path: str) -> bool: #vers 1
        """Parse IDE file and extract definitions
        
        Args:
            ide_path: Path to IDE file
            
        Returns:
            bool: True if parsing succeeded
        """
        try:
            if not os.path.exists(ide_path):
                self.parse_stats['errors'].append(f"IDE file not found: {ide_path}")
                return False
            
            self.file_path = ide_path
            
            with open(ide_path, 'r', encoding='ascii', errors='ignore') as f:
                content = f.read()
            
            return self.parse_ide_content(content)
            
        except Exception as e:
            error_msg = f"Error reading IDE file {ide_path}: {e}"
            self.parse_stats['errors'].append(error_msg)
            print(f"[ERROR] {error_msg}")
            return False
    
    def parse_ide_content(self, content: str) -> bool: #vers 1
        """Parse IDE content string
        
        Args:
            content: IDE file content as string
            
        Returns:
            bool: True if parsing succeeded
        """
        try:
            lines = content.split('\n')
            current_section = None
            
            self.parse_stats['total_lines'] = len(lines)
            
            for line_num, line in enumerate(lines, 1):
                line = line.strip()
                
                # Skip empty lines and comments
                if not line or line.startswith('#') or line.startswith('//'):
                    continue
                
                # Check for section headers
                section_name = line.lower()
                if section_name in ['objs', 'tobj', 'weap', 'hier', 'anim', 'cars', 'peds', 'path', 'ped', 'end']:
                    if section_name == 'end':
                        current_section = None
                    else:
                        current_section = section_name
                        if current_section not in self.sections:
                            self.sections[current_section] = []
                            self.parse_stats['sections_found'] += 1
                    continue
                
                # Parse section entries
                if current_section:
                    try:
                        if self._parse_section_entry(current_section, line, line_num):
                            self.parse_stats['parsed_lines'] += 1
                    except Exception as e:
                        error_msg = f"Error parsing line {line_num}: {e}"
                        self.parse_stats['errors'].append(error_msg)
                        print(f"[WARNING] {error_msg}")
            
            self.parse_stats['models_found'] = len(self.models)
            self._build_texture_relationships()
            
            return True
            
        except Exception as e:
            error_msg = f"Error parsing IDE content: {e}"
            self.parse_stats['errors'].append(error_msg)
            print(f"[ERROR] {error_msg}")
            return False
    
    def _parse_section_entry(self, section: str, line: str, line_num: int) -> bool: #vers 1
        """Parse individual section entry
        
        Args:
            section: Current section name
            line: Line content
            line_num: Line number for error reporting
            
        Returns:
            bool: True if parsing succeeded
        """
        try:
            parts = [part.strip() for part in line.split(',')]
            
            if section in ['objs', 'tobj']:
                # Objects: ID, ModelName, TxdName, [DrawDist, Flags, ...]
                if len(parts) >= 3:
                    model_id = int(parts[0])
                    model_name = parts[1].strip()
                    txd_name = parts[2].strip()
                    
                    self.models[model_id] = {
                        'name': model_name,
                        'txd': txd_name,
                        'dff': f"{model_name}.dff",
                        'type': 'object',
                        'section': section,
                        'line': line_num
                    }
                    
                    self.sections[section].append({
                        'id': model_id,
                        'model': model_name,
                        'txd': txd_name,
                        'line': line_num
                    })
                    return True
            
            elif section == 'cars':
                # Vehicles: ID, ModelName, TxdName, Type, HandlingId, ...
                if len(parts) >= 3:
                    model_id = int(parts[0])
                    model_name = parts[1].strip()
                    txd_name = parts[2].strip()
                    
                    self.models[model_id] = {
                        'name': model_name,
                        'txd': txd_name,
                        'dff': f"{model_name}.dff",
                        'type': 'vehicle',
                        'section': section,
                        'line': line_num
                    }
                    
                    self.sections[section].append({
                        'id': model_id,
                        'model': model_name,
                        'txd': txd_name,
                        'line': line_num
                    })
                    return True
            
            elif section in ['peds', 'ped']:
                # Pedestrians: ID, ModelName, TxdName, PedType, ...
                if len(parts) >= 3:
                    model_id = int(parts[0])
                    model_name = parts[1].strip()
                    txd_name = parts[2].strip()
                    
                    self.models[model_id] = {
                        'name': model_name,
                        'txd': txd_name,
                        'dff': f"{model_name}.dff",
                        'type': 'pedestrian',
                        'section': section,
                        'line': line_num
                    }
                    
                    self.sections[section].append({
                        'id': model_id,
                        'model': model_name,
                        'txd': txd_name,
                        'line': line_num
                    })
                    return True
            
            elif section == 'weap':
                # Weapons: ID, ModelName, TxdName, AnimName, ...
                if len(parts) >= 3:
                    model_id = int(parts[0])
                    model_name = parts[1].strip()
                    txd_name = parts[2].strip()
                    
                    self.models[model_id] = {
                        'name': model_name,
                        'txd': txd_name,
                        'dff': f"{model_name}.dff",
                        'type': 'weapon',
                        'section': section,
                        'line': line_num
                    }
                    
                    self.sections[section].append({
                        'id': model_id,
                        'model': model_name,
                        'txd': txd_name,
                        'line': line_num
                    })
                    return True
            
            return False
            
        except ValueError as e:
            # Invalid model ID (not a number)
            return False
        except Exception as e:
            print(f"[WARNING] Error parsing entry at line {line_num}: {e}")
            return False
    
    def _build_texture_relationships(self): #vers 1
        """Build texture -> models relationships"""
        for model_id, model_data in self.models.items():
            txd_name = model_data['txd']
            model_name = model_data['name']
            
            if txd_name not in self.textures:
                self.textures[txd_name] = []
            
            if model_name not in self.textures[txd_name]:
                self.textures[txd_name].append(model_name)
    
    def get_model_relationships(self) -> Dict[str, List[str]]: #vers 1
        """Get model -> texture relationships
        
        Returns:
            Dict mapping model names to their texture dictionary names
        """
        relationships = {}
        
        for model_id, model_data in self.models.items():
            model_name = model_data['name']
            txd_name = model_data['txd']
            
            if model_name not in relationships:
                relationships[model_name] = []
            
            if txd_name not in relationships[model_name]:
                relationships[model_name].append(txd_name)
        
        return relationships
    
    def get_texture_relationships(self) -> Dict[str, List[str]]: #vers 1
        """Get texture -> models relationships
        
        Returns:
            Dict mapping texture names to their model names
        """
        return self.textures.copy()
    
    def get_files_for_model(self, model_name: str) -> List[str]: #vers 1
        """Get all files associated with a model name
        
        Args:
            model_name: Name of the model
            
        Returns:
            List of filenames (DFF, TXD) associated with the model
        """
        files = []
        
        for model_id, model_data in self.models.items():
            if model_data['name'].lower() == model_name.lower():
                # Add DFF file
                files.append(model_data['dff'])
                # Add TXD file
                files.append(f"{model_data['txd']}.txd")
                break
        
        return files
    
    def get_files_for_texture(self, txd_name: str) -> List[str]: #vers 1
        """Get all files associated with a texture dictionary
        
        Args:
            txd_name: Name of the texture dictionary
            
        Returns:
            List of filenames (DFF, TXD) associated with the texture
        """
        files = []
        
        # Add the TXD file itself
        files.append(f"{txd_name}.txd")
        
        # Add all DFF files that use this texture
        for model_id, model_data in self.models.items():
            if model_data['txd'].lower() == txd_name.lower():
                files.append(model_data['dff'])
        
        return files
    
    def find_model_by_filename(self, filename: str) -> Optional[str]: #vers 1
        """Find model name by DFF filename
        
        Args:
            filename: DFF filename (e.g., "player.dff")
            
        Returns:
            Model name if found, None otherwise
        """
        filename_lower = filename.lower()
        
        for model_id, model_data in self.models.items():
            if model_data['dff'].lower() == filename_lower:
                return model_data['name']
        
        return None
    
    def find_texture_by_filename(self, filename: str) -> Optional[str]: #vers 1
        """Find texture name by TXD filename
        
        Args:
            filename: TXD filename (e.g., "player.txd")
            
        Returns:
            Texture name if found, None otherwise
        """
        filename_lower = filename.lower()
        
        # Extract name without extension
        if filename_lower.endswith('.txd'):
            txd_name = filename_lower[:-4]
            
            for model_id, model_data in self.models.items():
                if model_data['txd'].lower() == txd_name:
                    return model_data['txd']
        
        return None
    
    def get_summary(self) -> str: #vers 1
        """Get parsing summary as formatted string
        
        Returns:
            Formatted summary string
        """
        summary = []
        summary.append("IDE File Analysis Summary:")
        summary.append(f"File: {os.path.basename(self.file_path) if self.file_path else 'Unknown'}")
        summary.append(f"Total lines: {self.parse_stats['total_lines']}")
        summary.append(f"Parsed lines: {self.parse_stats['parsed_lines']}")
        summary.append(f"Models found: {self.parse_stats['models_found']}")
        summary.append(f"Sections found: {self.parse_stats['sections_found']}")
        summary.append("")
        
        if self.sections:
            summary.append("Sections:")
            for section, entries in self.sections.items():
                summary.append(f"  {section.upper()}: {len(entries)} entries")
        
        if self.parse_stats['errors']:
            summary.append("")
            summary.append("Errors:")
            for error in self.parse_stats['errors'][:5]:  # Show first 5 errors
                summary.append(f"  - {error}")
            if len(self.parse_stats['errors']) > 5:
                summary.append(f"  ... and {len(self.parse_stats['errors']) - 5} more errors")
        
        return "\n".join(summary)
    
    def get_status_text(self) -> str: #vers 1
        """Get status text for display in dialogs
        
        Returns:
            Status text like "540 IDE lines, showing 730 files"
        """
        ide_lines = self.parse_stats['parsed_lines']
        total_files = len(self.models) * 2  # Each model has DFF + TXD
        
        return f"📋 {ide_lines} IDE lines, showing {total_files} files"
    
    def clear(self): #vers 1
        """Clear all parsed data"""
        self.models.clear()
        self.textures.clear()
        self.sections.clear()
        self.file_path = ""
        self.parse_stats = {
            'total_lines': 0,
            'parsed_lines': 0,
            'models_found': 0,
            'sections_found': 0,
            'errors': []
        }

def parse_ide_file(ide_path: str) -> Optional[IDEParser]: #vers 1
    """Convenience function to parse an IDE file
    
    Args:
        ide_path: Path to IDE file
        
    Returns:
        IDEParser instance if successful, None otherwise
    """
    parser = IDEParser()
    if parser.parse_ide_file(ide_path):
        return parser
    return None

def integrate_ide_parser(main_window): #vers 1
    """Integrate IDE parser functions into main window
    
    Args:
        main_window: Main window instance
        
    Returns:
        bool: True if integration succeeded
    """
    try:
        # Add parser functions to main window
        main_window.parse_ide_file = parse_ide_file
        main_window.create_ide_parser = lambda: IDEParser()
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("✅ IDE parser functions integrated")
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error integrating IDE parser: {str(e)}")
        return False

__all__ = [
    'IDEParser',
    'parse_ide_file', 
    'integrate_ide_parser'
]