#!/usr/bin/env python3
"""
Wind Data Analyzer for Jamaica Bay Environmental Sensor
Analyzes wind speed, direction, and patterns from local environmental sensors.
"""

import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional
import warnings

warnings.filterwarnings('ignore')


class WindDataAnalyzer:
    """Analyzes wind data from environmental sensors."""
    
    def __init__(self, data: Optional[pd.DataFrame] = None):
        """
        Initialize the wind data analyzer.
        
        Args:
            data: DataFrame with columns ['timestamp', 'wind_speed', 'wind_direction']
        """
        self.data = data
        
    def load_data(self, filepath: str) -> pd.DataFrame:
        """
        Load wind data from a CSV file.
        
        Args:
            filepath: Path to CSV file
            
        Returns:
            DataFrame with wind data
        """
        self.data = pd.read_csv(filepath)
        if 'timestamp' in self.data.columns:
            self.data['timestamp'] = pd.to_datetime(self.data['timestamp'])
        return self.data
    
    def load_from_dict(self, data_dict: Dict) -> pd.DataFrame:
        """
        Load wind data from a dictionary.
        
        Args:
            data_dict: Dictionary with wind data
            
        Returns:
            DataFrame with wind data
        """
        self.data = pd.DataFrame(data_dict)
        if 'timestamp' in self.data.columns:
            self.data['timestamp'] = pd.to_datetime(self.data['timestamp'])
        return self.data
    
    def get_basic_statistics(self) -> Dict[str, float]:
        """
        Calculate basic statistics for wind data.
        
        Returns:
            Dictionary with mean, median, std, min, max wind speeds
        """
        if self.data is None or self.data.empty:
            raise ValueError("No data loaded")
        
        stats = {
            'mean_speed': self.data['wind_speed'].mean(),
            'median_speed': self.data['wind_speed'].median(),
            'std_speed': self.data['wind_speed'].std(),
            'min_speed': self.data['wind_speed'].min(),
            'max_speed': self.data['wind_speed'].max()
        }
        return stats
    
    def get_wind_rose_data(self, bins: int = 16) -> Dict[str, List]:
        """
        Prepare data for wind rose diagram.
        
        Args:
            bins: Number of directional bins
            
        Returns:
            Dictionary with directional bins and speed distributions
        """
        if self.data is None or self.data.empty:
            raise ValueError("No data loaded")
        
        # Create directional bins
        bin_size = 360 / bins
        self.data['direction_bin'] = (self.data['wind_direction'] / bin_size).astype(int) % bins
        
        wind_rose = {
            'directions': [],
            'frequencies': [],
            'avg_speeds': []
        }
        
        for i in range(bins):
            bin_data = self.data[self.data['direction_bin'] == i]
            direction = i * bin_size
            frequency = len(bin_data) / len(self.data) * 100
            avg_speed = bin_data['wind_speed'].mean() if len(bin_data) > 0 else 0
            
            wind_rose['directions'].append(direction)
            wind_rose['frequencies'].append(frequency)
            wind_rose['avg_speeds'].append(avg_speed)
        
        return wind_rose
    
    def detect_calm_periods(self, threshold: float = 2.0) -> pd.DataFrame:
        """
        Detect periods of calm wind (low speed).
        
        Args:
            threshold: Wind speed threshold for calm conditions (m/s)
            
        Returns:
            DataFrame with calm periods
        """
        if self.data is None or self.data.empty:
            raise ValueError("No data loaded")
        
        calm_data = self.data[self.data['wind_speed'] < threshold].copy()
        return calm_data
    
    def detect_strong_wind_events(self, threshold: float = 10.0) -> pd.DataFrame:
        """
        Detect strong wind events.
        
        Args:
            threshold: Wind speed threshold for strong wind (m/s)
            
        Returns:
            DataFrame with strong wind events
        """
        if self.data is None or self.data.empty:
            raise ValueError("No data loaded")
        
        strong_wind = self.data[self.data['wind_speed'] > threshold].copy()
        return strong_wind
    
    def calculate_gust_factor(self, window: int = 10) -> pd.Series:
        """
        Calculate gust factor (ratio of max to mean wind speed in window).
        
        Args:
            window: Rolling window size
            
        Returns:
            Series with gust factors
        """
        if self.data is None or self.data.empty:
            raise ValueError("No data loaded")
        
        rolling_max = self.data['wind_speed'].rolling(window=window).max()
        rolling_mean = self.data['wind_speed'].rolling(window=window).mean()
        gust_factor = rolling_max / rolling_mean
        
        return gust_factor
    
    def get_prevailing_direction(self) -> Tuple[float, float]:
        """
        Calculate the prevailing wind direction.
        
        Returns:
            Tuple of (prevailing direction in degrees, percentage of time)
        """
        if self.data is None or self.data.empty:
            raise ValueError("No data loaded")
        
        # Use 16-bin approach
        bin_size = 22.5
        self.data['direction_bin'] = (self.data['wind_direction'] / bin_size).round() % 16
        
        most_common_bin = self.data['direction_bin'].mode()[0]
        prevailing_direction = most_common_bin * bin_size
        percentage = (self.data['direction_bin'] == most_common_bin).sum() / len(self.data) * 100
        
        return prevailing_direction, percentage
    
    def analyze_daily_pattern(self) -> pd.DataFrame:
        """
        Analyze daily wind patterns (hourly averages).
        
        Returns:
            DataFrame with hourly statistics
        """
        if self.data is None or self.data.empty:
            raise ValueError("No data loaded")
        
        if 'timestamp' not in self.data.columns:
            raise ValueError("Timestamp column required for daily pattern analysis")
        
        self.data['hour'] = self.data['timestamp'].dt.hour
        hourly_stats = self.data.groupby('hour')['wind_speed'].agg(['mean', 'std', 'min', 'max'])
        
        return hourly_stats
    
    def calculate_power_density(self, air_density: float = 1.225) -> pd.Series:
        """
        Calculate wind power density (W/m²).
        
        Args:
            air_density: Air density in kg/m³ (default 1.225 at sea level)
            
        Returns:
            Series with power density values
        """
        if self.data is None or self.data.empty:
            raise ValueError("No data loaded")
        
        # Power density = 0.5 * air_density * wind_speed³
        power_density = 0.5 * air_density * (self.data['wind_speed'] ** 3)
        
        return power_density
    
    def get_summary_report(self) -> str:
        """
        Generate a comprehensive summary report.
        
        Returns:
            String with formatted report
        """
        if self.data is None or self.data.empty:
            raise ValueError("No data loaded")
        
        stats = self.get_basic_statistics()
        prevailing_dir, prevailing_pct = self.get_prevailing_direction()
        calm_periods = len(self.detect_calm_periods())
        strong_events = len(self.detect_strong_wind_events())
        
        report = f"""
Wind Data Analysis Report - Jamaica Bay
{'=' * 50}

Data Summary:
  Total Observations: {len(self.data)}
  
Wind Speed Statistics:
  Mean Speed: {stats['mean_speed']:.2f} m/s
  Median Speed: {stats['median_speed']:.2f} m/s
  Std Deviation: {stats['std_speed']:.2f} m/s
  Min Speed: {stats['min_speed']:.2f} m/s
  Max Speed: {stats['max_speed']:.2f} m/s

Wind Direction:
  Prevailing Direction: {prevailing_dir:.1f}°
  Prevailing Frequency: {prevailing_pct:.1f}%

Wind Events:
  Calm Periods (< 2 m/s): {calm_periods}
  Strong Wind Events (> 10 m/s): {strong_events}
  
Average Power Density: {self.calculate_power_density().mean():.2f} W/m²
"""
        return report
