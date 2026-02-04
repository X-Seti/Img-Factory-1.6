"""
Localization System for IMG Factory
Handles multi-language support for UI elements
"""
import os
import json
from typing import Dict, Optional
from pathlib import Path


class LocalizationManager:
    """
    Manages localization for the IMG Factory application
    """
    def __init__(self, locale_dir: str = None):
        self.locale_dir = locale_dir or os.path.join(os.path.dirname(__file__), 'locales')
        self.current_locale = 'en'
        self.translations = {}
        
        # Ensure locale directory exists
        os.makedirs(self.locale_dir, exist_ok=True)
        
        # Load default English translations
        self._load_default_translations()
        
    def _load_default_translations(self):
        """Load default English translations"""
        self.translations = {
            'en': {
                # Button labels
                'Create': 'Create',
                'Open': 'Open', 
                'Reload': 'Reload',
                'Close': 'Close',
                'Close All': 'Close All',
                'Rebuild': 'Rebuild',
                'Rebuild All': 'Rebuild All',
                'Save Entry': 'Save Entry',
                'Merge': 'Merge',
                'Split via': 'Split via',
                'Convert': 'Convert',
                'Import': 'Import',
                'Import via': 'Import via',
                'Refresh': 'Refresh',
                'Export': 'Export',
                'Export via': 'Export via',
                'Dump': 'Dump',
                'Remove': 'Remove',
                'Remove via': 'Remove via',
                'Extract': 'Extract',
                'Rename': 'Rename',
                'Select All': 'Select All',
                'Inverse': 'Inverse',
                'Search': 'Search',
                'Sort via': 'Sort via',
                'Pin selected': 'Pin selected',
                
                # Editor labels
                'Col Edit': 'Col Edit',
                'Txd Edit': 'Txd Edit',
                'Dff Edit': 'Dff Edit',
                'Ipf Edit': 'Ipf Edit',
                'IDE Edit': 'IDE Edit',
                'IPL Edit': 'IPL Edit',
                'Dat Edit': 'Dat Edit',
                'Zons Cull Ed': 'Zons Cull Ed',
                'Weap Edit': 'Weap Edit',
                'Vehi Edit': 'Vehi Edit',
                'Peds Edit': 'Peds Edit',
                'Radar Map': 'Radar Map',
                'Paths Map': 'Paths Map',
                'Waterpro': 'Waterpro',
                'Weather': 'Weather',
                'Handling': 'Handling',
                'Objects': 'Objects',
                'SCM code': 'SCM code',
                'GXT font': 'GXT font',
                'Menu Edit': 'Menu Edit',
                
                # Group labels
                'IMG, COL, TXD Files': 'IMG, COL, TXD Files',
                'File Entries': 'File Entries',
                'Editing Options': 'Editing Options',
                
                # Other UI elements
                'Activity Log': 'Activity Log',
                'Ready': 'Ready',
            }
        }
        
    def load_locale(self, locale_code: str) -> bool:
        """Load a specific locale from JSON file"""
        locale_file = os.path.join(self.locale_dir, f'{locale_code}.json')
        
        if os.path.exists(locale_file):
            try:
                with open(locale_file, 'r', encoding='utf-8') as f:
                    self.translations[locale_code] = json.load(f)
                self.current_locale = locale_code
                return True
            except Exception as e:
                print(f"Error loading locale {locale_code}: {e}")
                return False
        return False
        
    def get_translation(self, text: str, locale: str = None) -> str:
        """Get translation for a text in the specified locale"""
        locale = locale or self.current_locale
        
        if locale in self.translations and text in self.translations[locale]:
            return self.translations[locale][text]
        else:
            # Fallback to English if translation not found
            if 'en' in self.translations and text in self.translations['en']:
                return self.translations['en'][text]
            else:
                # Return original text if no translation exists
                return text
                
    def translate_button_text(self, text: str) -> str:
        """Specifically translate button text"""
        return self.get_translation(text, self.current_locale)
        
    def get_available_locales(self) -> list:
        """Get list of available locales"""
        locales = ['en']  # Always include English as default
        for file in os.listdir(self.locale_dir):
            if file.endswith('.json'):
                locale_code = file[:-5]  # Remove .json extension
                if locale_code not in locales:
                    locales.append(locale_code)
        return locales


# Global localization manager instance
localization_manager = LocalizationManager()


def tr(text: str) -> str:
    """Shortcut function for translation"""
    return localization_manager.get_translation(text)


def tr_button(text: str) -> str:
    """Shortcut function for button translation"""
    return localization_manager.translate_button_text(text)
