#!/usr/bin/env python3
"""
Example usage of the Jamaica Bay Wind Data Analyzer.
"""

from wind_analyzer import WindDataAnalyzer
from visualizer import WindDataVisualizer
from generate_sample_data import generate_sample_data


def main():
    """Run a complete example analysis."""
    
    print("=" * 60)
    print("Jamaica Bay Wind Data Analysis Example")
    print("=" * 60)
    print()
    
    # Generate sample data
    print("Generating sample wind data...")
    data = generate_sample_data(num_samples=720)  # 30 days of hourly data
    print(f"Generated {len(data)} samples\n")
    
    # Initialize analyzer
    analyzer = WindDataAnalyzer(data)
    
    # Get basic statistics
    print("Basic Statistics:")
    print("-" * 60)
    stats = analyzer.get_basic_statistics()
    for key, value in stats.items():
        print(f"  {key.replace('_', ' ').title()}: {value:.2f} m/s")
    print()
    
    # Get prevailing direction
    print("Prevailing Wind Direction:")
    print("-" * 60)
    direction, percentage = analyzer.get_prevailing_direction()
    print(f"  Direction: {direction:.1f}°")
    print(f"  Frequency: {percentage:.1f}%")
    print()
    
    # Detect wind events
    print("Wind Events:")
    print("-" * 60)
    calm = analyzer.detect_calm_periods(threshold=2.0)
    strong = analyzer.detect_strong_wind_events(threshold=10.0)
    print(f"  Calm periods (< 2 m/s): {len(calm)}")
    print(f"  Strong wind events (> 10 m/s): {len(strong)}")
    print()
    
    # Calculate power density
    print("Wind Power Analysis:")
    print("-" * 60)
    power_density = analyzer.calculate_power_density()
    print(f"  Average Power Density: {power_density.mean():.2f} W/m²")
    print(f"  Max Power Density: {power_density.max():.2f} W/m²")
    print()
    
    # Get wind rose data
    print("Wind Rose Analysis:")
    print("-" * 60)
    wind_rose = analyzer.get_wind_rose_data(bins=16)
    print(f"  Directional bins: {len(wind_rose['directions'])}")
    print()
    
    # Daily pattern analysis
    print("Daily Pattern Analysis:")
    print("-" * 60)
    daily_pattern = analyzer.analyze_daily_pattern()
    print(daily_pattern.head(10))
    print()
    
    # Generate comprehensive report
    print("\nGenerating comprehensive report...")
    print(analyzer.get_summary_report())
    
    # Create visualizations
    print("\nGenerating visualizations...")
    visualizer = WindDataVisualizer()
    
    print("  1. Time series plot...")
    visualizer.plot_time_series(data, save_path='wind_timeseries.png')
    
    print("  2. Wind rose diagram...")
    visualizer.plot_wind_rose(wind_rose, save_path='wind_rose.png')
    
    print("  3. Speed distribution...")
    visualizer.plot_speed_distribution(data, save_path='speed_distribution.png')
    
    print("  4. Daily pattern...")
    visualizer.plot_daily_pattern(daily_pattern, save_path='daily_pattern.png')
    
    print("  5. Power density...")
    visualizer.plot_power_density(data, power_density, save_path='power_density.png')
    
    print("\nAnalysis complete! Visualizations saved.")
    print("\nGenerated files:")
    print("  - wind_timeseries.png")
    print("  - wind_rose.png")
    print("  - speed_distribution.png")
    print("  - daily_pattern.png")
    print("  - power_density.png")


if __name__ == '__main__':
    main()
