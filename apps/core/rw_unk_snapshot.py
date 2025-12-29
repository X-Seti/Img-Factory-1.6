#this belongs in components/rw_unk_snapshot.py - Version: 4
# X-Seti - July20 2025 - IMG Factory 1.5 - Unknown RW File Snapshotter
# Captures unknown RW files for analysis and database expansion

"""
Unknown RW File Snapshotter
Automatically captures unknown RenderWare files for analysis
Creates snapshots and displays analysis in status window
"""

import os
import struct
import shutil
from typing import Dict, List, Optional, Tuple, Any
from pathlib import Path
from datetime import datetime

# Import existing functions - NO NEW FUNCTIONALITY
from apps.methods.rw_versions import get_rw_version_name, is_valid_rw_version
from apps.methods.img_core_classes import IMGFile, IMGEntry

##Methods list -
# capture_unknown_rw_files
# create_snapshot_folder
# extract_rw_header_info
# generate_snapshot_report
# is_unknown_rw_file
# save_file_snapshot
# update_status_window

##Classes -
# RWSnapshotManager
# UnknownRWFile

class UnknownRWFile:
    """Container for unknown RW file information"""
    
    def __init__(self, entry: IMGEntry, img_file: IMGFile): #vers 1
        self.entry_name = entry.name
        self.file_size = entry.size
        self.offset = entry.offset
        self.img_source = os.path.basename(img_file.file_path)
        self.platform = getattr(img_file, 'platform', 'PC')
        self.header_data = None
        self.rw_version_raw = None
        self.rw_version_hex = None
        self.file_signature = None
        self.analysis_info = {}

class RWSnapshotManager:
    """Manages unknown RW file snapshots and analysis"""
    
    def __init__(self, main_window): #vers 1
        self.main_window = main_window
        self.snapshot_folder = Path("snapshots")
        self.max_snapshots = 10
        self.unknown_files = []
        self.snapshot_count = 0
        
    def capture_unknown_rw_files(self, img_file: IMGFile) -> List[UnknownRWFile]: #vers 1
        """Scan IMG file for unknown RW files and capture them"""
        unknown_files = []
        
        try:
            if not img_file or not img_file.entries:
                return unknown_files
            
            print(f"[DEBUG] Scanning {len(img_file.entries)} entries for unknown RW files...")
            
            for entry in img_file.entries:
                if self.is_unknown_rw_file(entry, img_file):
                    unknown_file = UnknownRWFile(entry, img_file)
                    
                    # Extract header information
                    header_info = self.extract_rw_header_info(entry, img_file)
                    if header_info:
                        unknown_file.header_data = header_info['header_data']
                        unknown_file.rw_version_raw = header_info['rw_version_raw']
                        unknown_file.rw_version_hex = header_info['rw_version_hex']
                        unknown_file.file_signature = header_info['file_signature']
                        unknown_file.analysis_info = header_info['analysis']
                        
                        unknown_files.append(unknown_file)
                        
                        # Stop at max snapshots
                        if len(unknown_files) >= self.max_snapshots:
                            break
            
            if unknown_files:
                print(f"[SUCCESS] Found {len(unknown_files)} unknown RW files")
                self.unknown_files.extend(unknown_files)
                self.create_snapshots(unknown_files, img_file)
                self.update_status_window(unknown_files)
            else:
                print(f"[INFO] No unknown RW files found in {img_file.file_path}")
                
        except Exception as e:
            print(f"[ERROR] Error capturing unknown RW files: {e}")
            
        return unknown_files

    def is_unknown_rw_file(self, entry: IMGEntry, img_file: IMGFile) -> bool: #vers 1
        """Check if entry is an unknown RenderWare file"""
        try:
            # Only check DFF and TXD files
            if not hasattr(entry, 'extension') or entry.extension not in ['DFF', 'TXD']:
                return False
                
            # Check if RW version is unknown
            if hasattr(entry, 'rw_version') and entry.rw_version > 0:
                version_name = get_rw_version_name(entry.rw_version)
                # If version name contains "Unknown" or starts with hex, it's unknown
                if "Unknown" in version_name or version_name.startswith("0x"):
                    return True
                    
            # Also check if version detection failed entirely
            if hasattr(entry, 'get_version_text') and callable(entry.get_version_text):
                version_text = entry.get_version_text()
                if version_text in ["RW Unknown", "Unknown"]:
                    return True
                    
            return False
            
        except Exception as e:
            print(f"[WARNING] Error checking unknown RW file {entry.name}: {e}")
            return False

    def extract_rw_header_info(self, entry: IMGEntry, img_file: IMGFile) -> Optional[Dict[str, Any]]: #vers 1
        """Extract detailed RW header information from file"""
        try:
            # Read file header (first 32 bytes for analysis)
            if img_file.version.value == 1:  # VERSION_1
                img_path = img_file.file_path.replace('.dir', '.img')
                if not os.path.exists(img_path):
                    return None
                file_path = img_path
            else:
                file_path = img_file.file_path
                
            with open(file_path, 'rb') as f:
                f.seek(entry.offset)
                header_data = f.read(min(32, entry.size))
                
            if len(header_data) < 12:
                return None
                
            # Parse RW header structure
            analysis = {}
            
            # File signature (first 4 bytes)
            file_signature = header_data[:4]
            analysis['file_signature'] = file_signature.hex().upper()
            
            # File size (bytes 4-8)
            if len(header_data) >= 8:
                file_size = struct.unpack('<I', header_data[4:8])[0]
                analysis['header_file_size'] = file_size
                
            # RW version (bytes 8-12)
            rw_version_raw = None
            rw_version_hex = None
            if len(header_data) >= 12:
                rw_version_raw = struct.unpack('<I', header_data[8:12])[0]
                rw_version_hex = f"0x{rw_version_raw:08X}"
                analysis['rw_version_raw'] = rw_version_raw
                analysis['rw_version_hex'] = rw_version_hex
                analysis['is_valid_rw_range'] = is_valid_rw_version(rw_version_raw)
                
            # Additional analysis
            analysis['entry_size'] = entry.size
            analysis['entry_offset'] = f"0x{entry.offset:08X}"
            analysis['platform'] = getattr(img_file, 'platform', 'PC')
            
            return {
                'header_data': header_data,
                'rw_version_raw': rw_version_raw,
                'rw_version_hex': rw_version_hex,
                'file_signature': file_signature,
                'analysis': analysis
            }
            
        except Exception as e:
            print(f"[ERROR] Error extracting RW header for {entry.name}: {e}")
            return None

    def create_snapshots(self, unknown_files: List[UnknownRWFile], img_file: IMGFile): #vers 1
        """Create file snapshots for analysis"""
        try:
            # Create snapshot folder
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            session_folder = self.snapshot_folder / f"session_{timestamp}"
            session_folder.mkdir(parents=True, exist_ok=True)
            
            print(f"[DEBUG] Creating snapshots in {session_folder}")
            
            for i, unknown_file in enumerate(unknown_files):
                try:
                    # Save file snapshot
                    self.save_file_snapshot(unknown_file, img_file, session_folder, i)
                    self.snapshot_count += 1
                    
                except Exception as e:
                    print(f"[ERROR] Error creating snapshot for {unknown_file.entry_name}: {e}")
                    
            # Generate analysis report
            self.generate_snapshot_report(unknown_files, session_folder)
            
            print(f"[SUCCESS] Created {len(unknown_files)} snapshots in {session_folder}")
            
        except Exception as e:
            print(f"[ERROR] Error creating snapshots: {e}")

    def save_file_snapshot(self, unknown_file: UnknownRWFile, img_file: IMGFile, 
                          session_folder: Path, index: int): #vers 1
        """Save individual file snapshot"""
        try:
            # Create safe filename
            safe_name = "".join(c for c in unknown_file.entry_name if c.isalnum() or c in '._-')
            snapshot_name = f"{index:02d}_{safe_name}"
            snapshot_path = session_folder / snapshot_name
            
            # Extract and save file
            if img_file.version.value == 1:  # VERSION_1
                img_path = img_file.file_path.replace('.dir', '.img')
                source_path = img_path
            else:
                source_path = img_file.file_path
                
            with open(source_path, 'rb') as source:
                source.seek(unknown_file.offset)
                file_data = source.read(unknown_file.file_size)
                
            with open(snapshot_path, 'wb') as snapshot:
                snapshot.write(file_data)
                
            print(f"[DEBUG] Saved snapshot: {snapshot_name}")
            
        except Exception as e:
            print(f"[ERROR] Error saving snapshot for {unknown_file.entry_name}: {e}")

    def generate_snapshot_report(self, unknown_files: List[UnknownRWFile], session_folder: Path): #vers 1
        """Generate analysis report for snapshots"""
        try:
            report_path = session_folder / "analysis_report.txt"
            
            with open(report_path, 'w') as report:
                report.write("=" * 60 + "\n")
                report.write("IMG Factory 1.5 - Unknown RW Files Analysis Report\n")
                report.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
                report.write("=" * 60 + "\n\n")
                
                for i, unknown_file in enumerate(unknown_files):
                    report.write(f"File {i+1:02d}: {unknown_file.entry_name}\n")
                    report.write("-" * 40 + "\n")
                    report.write(f"Source IMG: {unknown_file.img_source}\n")
                    report.write(f"Platform: {unknown_file.platform}\n")
                    report.write(f"File Size: {unknown_file.file_size:,} bytes\n")
                    report.write(f"Offset: 0x{unknown_file.offset:08X}\n")
                    
                    if unknown_file.analysis_info:
                        analysis = unknown_file.analysis_info
                        report.write(f"File Signature: {analysis.get('file_signature', 'N/A')}\n")
                        report.write(f"RW Version Raw: {analysis.get('rw_version_raw', 'N/A')}\n")
                        report.write(f"RW Version Hex: {analysis.get('rw_version_hex', 'N/A')}\n")
                        report.write(f"Valid RW Range: {analysis.get('is_valid_rw_range', 'N/A')}\n")
                        report.write(f"Header File Size: {analysis.get('header_file_size', 'N/A')}\n")
                        
                    report.write("\n")
                    
            print(f"[SUCCESS] Generated analysis report: {report_path}")
            
        except Exception as e:
            print(f"[ERROR] Error generating report: {e}")

    def update_status_window(self, unknown_files: List[UnknownRWFile]): #vers 1
        """Update status window with unknown RW file information"""
        try:
            if not hasattr(self.main_window, 'log_message'):
                # Fallback to print if no log_message method
                self.print_unknown_files_info(unknown_files)
                return
                
            self.main_window.log_message("=" * 50)
            self.main_window.log_message("UNKNOWN RW FILES DETECTED")
            self.main_window.log_message("=" * 50)
            
            for i, unknown_file in enumerate(unknown_files):
                rw_hex = unknown_file.rw_version_hex or "N/A"
                sig = unknown_file.analysis_info.get('file_signature', 'N/A')
                
                # Format for easy copy-paste
                copy_paste_line = f"{unknown_file.entry_name} | RW: {rw_hex} | Sig: {sig} | Platform: {unknown_file.platform}"
                self.main_window.log_message(f"{i+1:02d}. {copy_paste_line}")
                
            self.main_window.log_message("=" * 50)
            self.main_window.log_message(f"Snapshots saved to: snapshots/session_{datetime.now().strftime('%Y%m%d_%H%M%S')}/")
            self.main_window.log_message("Copy the lines above for RW version database expansion")
            self.main_window.log_message("=" * 50)
            
        except Exception as e:
            print(f"[ERROR] Error updating status window: {e}")
            self.print_unknown_files_info(unknown_files)

    def print_unknown_files_info(self, unknown_files: List[UnknownRWFile]): #vers 1
        """Fallback: Print unknown files info to console"""
        print("=" * 50)
        print("🔍 UNKNOWN RW FILES DETECTED")
        print("=" * 50)
        
        for i, unknown_file in enumerate(unknown_files):
            rw_hex = unknown_file.rw_version_hex or "N/A"
            sig = unknown_file.analysis_info.get('file_signature', 'N/A')
            
            # Format for easy copy-paste
            copy_paste_line = f"{unknown_file.entry_name} | RW: {rw_hex} | Sig: {sig} | Platform: {unknown_file.platform}"
            print(f"{i+1:02d}. {copy_paste_line}")
            
        print("=" * 50)
        print(f"Snapshots saved to: snapshots/")
        print("Copy the lines above for RW version database expansion")
        print("=" * 50)

# Integration function for main IMG loading
def integrate_unknown_rw_detection(main_window): #vers 1
    """Integrate unknown RW detection into main IMG loading process"""
    try:
        # Create snapshot manager
        snapshot_manager = RWSnapshotManager(main_window)
        
        # Store reference in main window
        main_window.rw_snapshot_manager = snapshot_manager
        
        # Add method to main window for easy access
        def capture_unknown_rw_files_from_current_img():
            if hasattr(main_window, 'current_img') and main_window.current_img:
                return snapshot_manager.capture_unknown_rw_files(main_window.current_img)
            return []
            
        main_window.capture_unknown_rw_files = capture_unknown_rw_files_from_current_img
        
        print("Unknown RW detection integrated successfully")
        return True
        
    except Exception as e:
        print(f"Error integrating unknown RW detection: {e}")
        return False

# Export functions
__all__ = [
    'RWSnapshotManager',
    'UnknownRWFile',
    'integrate_unknown_rw_detection'
]
