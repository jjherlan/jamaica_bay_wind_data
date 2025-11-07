#!/usr/bin/env python3
"""
Visualization utilities for wind data analysis.
"""

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from typing import Optional, Tuple


class WindDataVisualizer:
    """Creates visualizations for wind data analysis."""
    
    def __init__(self, figsize: Tuple[int, int] = (12, 8)):
        """
        Initialize the visualizer.
        
        Args:
            figsize: Default figure size for plots
        """
        self.figsize = figsize
        
    def plot_time_series(self, data: pd.DataFrame, save_path: Optional[str] = None):
        """
        Plot wind speed and direction time series.
        
        Args:
            data: DataFrame with timestamp, wind_speed, wind_direction
            save_path: Optional path to save the figure
        """
        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=self.figsize)
        
        # Wind speed
        ax1.plot(data['timestamp'], data['wind_speed'], color='blue', linewidth=0.8)
        ax1.set_ylabel('Wind Speed (m/s)', fontsize=12)
        ax1.set_title('Wind Speed Time Series', fontsize=14, fontweight='bold')
        ax1.grid(True, alpha=0.3)
        
        # Wind direction
        ax2.scatter(data['timestamp'], data['wind_direction'], c=data['wind_speed'], 
                   cmap='viridis', s=10, alpha=0.6)
        ax2.set_ylabel('Wind Direction (°)', fontsize=12)
        ax2.set_xlabel('Time', fontsize=12)
        ax2.set_title('Wind Direction Time Series', fontsize=14, fontweight='bold')
        ax2.set_ylim(0, 360)
        ax2.grid(True, alpha=0.3)
        
        plt.colorbar(ax2.collections[0], ax=ax2, label='Wind Speed (m/s)')
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
        else:
            plt.show()
            
        plt.close()
    
    def plot_wind_rose(self, wind_rose_data: dict, save_path: Optional[str] = None):
        """
        Create a wind rose diagram.
        
        Args:
            wind_rose_data: Dictionary from WindDataAnalyzer.get_wind_rose_data()
            save_path: Optional path to save the figure
        """
        fig = plt.figure(figsize=(10, 10))
        ax = fig.add_subplot(111, projection='polar')
        
        directions = np.deg2rad(wind_rose_data['directions'])
        frequencies = wind_rose_data['frequencies']
        avg_speeds = wind_rose_data['avg_speeds']
        
        # Create bars
        width = 2 * np.pi / len(directions)
        bars = ax.bar(directions, frequencies, width=width, bottom=0.0)
        
        # Color bars by average speed
        colors = plt.cm.viridis(np.array(avg_speeds) / max(avg_speeds) if max(avg_speeds) > 0 else 0)
        for bar, color in zip(bars, colors):
            bar.set_facecolor(color)
            bar.set_alpha(0.8)
        
        ax.set_theta_zero_location('N')
        ax.set_theta_direction(-1)
        ax.set_title('Wind Rose Diagram', fontsize=14, fontweight='bold', pad=20)
        ax.set_ylabel('Frequency (%)', fontsize=10)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
        else:
            plt.show()
            
        plt.close()
    
    def plot_speed_distribution(self, data: pd.DataFrame, save_path: Optional[str] = None):
        """
        Plot wind speed distribution histogram.
        
        Args:
            data: DataFrame with wind_speed column
            save_path: Optional path to save the figure
        """
        fig, ax = plt.subplots(figsize=(10, 6))
        
        ax.hist(data['wind_speed'], bins=30, color='skyblue', edgecolor='black', alpha=0.7)
        ax.axvline(data['wind_speed'].mean(), color='red', linestyle='--', 
                   linewidth=2, label=f'Mean: {data["wind_speed"].mean():.2f} m/s')
        ax.axvline(data['wind_speed'].median(), color='green', linestyle='--', 
                   linewidth=2, label=f'Median: {data["wind_speed"].median():.2f} m/s')
        
        ax.set_xlabel('Wind Speed (m/s)', fontsize=12)
        ax.set_ylabel('Frequency', fontsize=12)
        ax.set_title('Wind Speed Distribution', fontsize=14, fontweight='bold')
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
        else:
            plt.show()
            
        plt.close()
    
    def plot_daily_pattern(self, hourly_stats: pd.DataFrame, save_path: Optional[str] = None):
        """
        Plot daily wind pattern (hourly averages).
        
        Args:
            hourly_stats: DataFrame from WindDataAnalyzer.analyze_daily_pattern()
            save_path: Optional path to save the figure
        """
        fig, ax = plt.subplots(figsize=(12, 6))
        
        hours = hourly_stats.index
        means = hourly_stats['mean']
        stds = hourly_stats['std']
        
        ax.plot(hours, means, color='blue', linewidth=2, marker='o', label='Mean')
        ax.fill_between(hours, means - stds, means + stds, alpha=0.3, color='blue', 
                        label='± 1 Std Dev')
        
        ax.set_xlabel('Hour of Day', fontsize=12)
        ax.set_ylabel('Wind Speed (m/s)', fontsize=12)
        ax.set_title('Daily Wind Pattern', fontsize=14, fontweight='bold')
        ax.set_xticks(range(0, 24))
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
        else:
            plt.show()
            
        plt.close()
    
    def plot_power_density(self, data: pd.DataFrame, power_density: pd.Series, 
                          save_path: Optional[str] = None):
        """
        Plot wind power density over time.
        
        Args:
            data: DataFrame with timestamp column
            power_density: Series with power density values
            save_path: Optional path to save the figure
        """
        fig, ax = plt.subplots(figsize=self.figsize)
        
        if 'timestamp' in data.columns:
            ax.plot(data['timestamp'], power_density, color='orange', linewidth=0.8)
            ax.set_xlabel('Time', fontsize=12)
        else:
            ax.plot(power_density, color='orange', linewidth=0.8)
            ax.set_xlabel('Sample Index', fontsize=12)
        
        ax.axhline(power_density.mean(), color='red', linestyle='--', 
                  linewidth=2, label=f'Mean: {power_density.mean():.2f} W/m²')
        
        ax.set_ylabel('Power Density (W/m²)', fontsize=12)
        ax.set_title('Wind Power Density', fontsize=14, fontweight='bold')
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
        else:
            plt.show()
            
        plt.close()
