#!/usr/bin/env python3
"""
Generate sample wind data for Jamaica Bay environmental sensor.
"""

import numpy as np
import pandas as pd
from datetime import datetime, timedelta


def generate_sample_data(num_samples: int = 1000, start_date: str = '2024-01-01') -> pd.DataFrame:
    """
    Generate realistic sample wind data.
    
    Args:
        num_samples: Number of data points to generate
        start_date: Starting date for the time series
        
    Returns:
        DataFrame with timestamp, wind_speed, wind_direction
    """
    np.random.seed(42)
    
    # Generate timestamps (hourly data)
    start = pd.to_datetime(start_date)
    timestamps = [start + timedelta(hours=i) for i in range(num_samples)]
    
    # Generate wind speed with realistic patterns
    # Base speed with daily cycle and random variation
    hours = np.array([ts.hour for ts in timestamps])
    
    # Daily pattern: higher winds during afternoon
    daily_pattern = 3 + 2 * np.sin(2 * np.pi * (hours - 6) / 24)
    
    # Add random variation and occasional gusts
    random_component = np.random.normal(0, 1.5, num_samples)
    gusts = np.random.choice([0, 1], size=num_samples, p=[0.95, 0.05]) * np.random.uniform(3, 8, num_samples)
    
    wind_speed = np.maximum(0, daily_pattern + random_component + gusts)
    
    # Generate wind direction with prevailing direction (SW = 225°)
    prevailing_direction = 225
    direction_variation = np.random.normal(0, 45, num_samples)
    wind_direction = (prevailing_direction + direction_variation) % 360
    
    # Create DataFrame
    data = pd.DataFrame({
        'timestamp': timestamps,
        'wind_speed': wind_speed,
        'wind_direction': wind_direction
    })
    
    return data


def save_sample_data(filepath: str = 'sample_wind_data.csv', num_samples: int = 1000):
    """
    Generate and save sample wind data to CSV.
    
    Args:
        filepath: Path to save CSV file
        num_samples: Number of samples to generate
    """
    data = generate_sample_data(num_samples)
    data.to_csv(filepath, index=False)
    print(f"Sample data saved to {filepath}")
    print(f"Generated {len(data)} samples from {data['timestamp'].min()} to {data['timestamp'].max()}")


if __name__ == '__main__':
    save_sample_data()
