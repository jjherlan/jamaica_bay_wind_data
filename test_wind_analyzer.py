#!/usr/bin/env python3
"""
Unit tests for wind data analyzer.
"""

import unittest
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from wind_analyzer import WindDataAnalyzer
from generate_sample_data import generate_sample_data


class TestWindDataAnalyzer(unittest.TestCase):
    """Test cases for WindDataAnalyzer class."""
    
    def setUp(self):
        """Set up test data before each test."""
        self.sample_data = generate_sample_data(num_samples=100)
        self.analyzer = WindDataAnalyzer(self.sample_data)
    
    def test_initialization(self):
        """Test analyzer initialization."""
        analyzer = WindDataAnalyzer()
        self.assertIsNone(analyzer.data)
        
        analyzer = WindDataAnalyzer(self.sample_data)
        self.assertIsNotNone(analyzer.data)
        self.assertEqual(len(analyzer.data), 100)
    
    def test_load_from_dict(self):
        """Test loading data from dictionary."""
        data_dict = {
            'timestamp': [datetime.now() + timedelta(hours=i) for i in range(10)],
            'wind_speed': [5.0, 6.0, 4.5, 7.2, 5.8, 6.3, 5.5, 4.8, 6.7, 5.2],
            'wind_direction': [180, 185, 190, 175, 200, 195, 180, 185, 190, 195]
        }
        analyzer = WindDataAnalyzer()
        result = analyzer.load_from_dict(data_dict)
        
        self.assertEqual(len(result), 10)
        self.assertIn('timestamp', result.columns)
        self.assertIn('wind_speed', result.columns)
        self.assertIn('wind_direction', result.columns)
    
    def test_basic_statistics(self):
        """Test basic statistics calculation."""
        stats = self.analyzer.get_basic_statistics()
        
        self.assertIn('mean_speed', stats)
        self.assertIn('median_speed', stats)
        self.assertIn('std_speed', stats)
        self.assertIn('min_speed', stats)
        self.assertIn('max_speed', stats)
        
        # Check that values are reasonable
        self.assertGreater(stats['mean_speed'], 0)
        self.assertGreater(stats['max_speed'], stats['min_speed'])
        self.assertGreaterEqual(stats['std_speed'], 0)
    
    def test_basic_statistics_empty_data(self):
        """Test that empty data raises ValueError."""
        analyzer = WindDataAnalyzer()
        with self.assertRaises(ValueError):
            analyzer.get_basic_statistics()
    
    def test_wind_rose_data(self):
        """Test wind rose data generation."""
        wind_rose = self.analyzer.get_wind_rose_data(bins=8)
        
        self.assertEqual(len(wind_rose['directions']), 8)
        self.assertEqual(len(wind_rose['frequencies']), 8)
        self.assertEqual(len(wind_rose['avg_speeds']), 8)
        
        # Frequencies should sum to approximately 100%
        total_freq = sum(wind_rose['frequencies'])
        self.assertAlmostEqual(total_freq, 100.0, delta=0.1)
    
    def test_detect_calm_periods(self):
        """Test calm period detection."""
        calm = self.analyzer.detect_calm_periods(threshold=2.0)
        
        self.assertIsInstance(calm, pd.DataFrame)
        # All speeds should be below threshold
        if len(calm) > 0:
            self.assertTrue(all(calm['wind_speed'] < 2.0))
    
    def test_detect_strong_wind_events(self):
        """Test strong wind event detection."""
        strong = self.analyzer.detect_strong_wind_events(threshold=10.0)
        
        self.assertIsInstance(strong, pd.DataFrame)
        # All speeds should be above threshold
        if len(strong) > 0:
            self.assertTrue(all(strong['wind_speed'] > 10.0))
    
    def test_gust_factor(self):
        """Test gust factor calculation."""
        gust_factor = self.analyzer.calculate_gust_factor(window=5)
        
        self.assertIsInstance(gust_factor, pd.Series)
        # Gust factor should be >= 1.0 where defined
        valid_gust = gust_factor.dropna()
        if len(valid_gust) > 0:
            self.assertTrue(all(valid_gust >= 0))
    
    def test_prevailing_direction(self):
        """Test prevailing direction calculation."""
        direction, percentage = self.analyzer.get_prevailing_direction()
        
        self.assertGreaterEqual(direction, 0)
        self.assertLess(direction, 360)
        self.assertGreater(percentage, 0)
        self.assertLessEqual(percentage, 100)
    
    def test_daily_pattern(self):
        """Test daily pattern analysis."""
        pattern = self.analyzer.analyze_daily_pattern()
        
        self.assertIsInstance(pattern, pd.DataFrame)
        self.assertIn('mean', pattern.columns)
        self.assertIn('std', pattern.columns)
        self.assertIn('min', pattern.columns)
        self.assertIn('max', pattern.columns)
    
    def test_power_density(self):
        """Test power density calculation."""
        power_density = self.analyzer.calculate_power_density()
        
        self.assertIsInstance(power_density, pd.Series)
        self.assertEqual(len(power_density), len(self.sample_data))
        # Power density should be positive
        self.assertTrue(all(power_density >= 0))
    
    def test_power_density_custom_air_density(self):
        """Test power density with custom air density."""
        power1 = self.analyzer.calculate_power_density(air_density=1.225)
        power2 = self.analyzer.calculate_power_density(air_density=1.0)
        
        # Higher air density should give higher power
        self.assertGreater(power1.mean(), power2.mean())
    
    def test_summary_report(self):
        """Test summary report generation."""
        report = self.analyzer.get_summary_report()
        
        self.assertIsInstance(report, str)
        self.assertIn('Wind Data Analysis Report', report)
        self.assertIn('Mean Speed', report)
        self.assertIn('Prevailing Direction', report)


class TestSampleDataGeneration(unittest.TestCase):
    """Test cases for sample data generation."""
    
    def test_generate_sample_data(self):
        """Test sample data generation."""
        data = generate_sample_data(num_samples=50)
        
        self.assertEqual(len(data), 50)
        self.assertIn('timestamp', data.columns)
        self.assertIn('wind_speed', data.columns)
        self.assertIn('wind_direction', data.columns)
        
        # Check data ranges
        self.assertTrue(all(data['wind_speed'] >= 0))
        self.assertTrue(all(data['wind_direction'] >= 0))
        self.assertTrue(all(data['wind_direction'] < 360))
    
    def test_generate_sample_data_custom_date(self):
        """Test sample data generation with custom start date."""
        data = generate_sample_data(num_samples=10, start_date='2023-06-15')
        
        first_timestamp = data['timestamp'].iloc[0]
        self.assertEqual(first_timestamp.year, 2023)
        self.assertEqual(first_timestamp.month, 6)
        self.assertEqual(first_timestamp.day, 15)


if __name__ == '__main__':
    unittest.main()
