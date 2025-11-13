# Jamaica Bay Wind Data Analyzer

A comprehensive Python toolkit for analyzing wind data from local environmental sensors in Jamaica Bay. This project provides tools for statistical analysis, visualization, and pattern detection in wind speed and direction data.

## Features

- **Statistical Analysis**: Calculate mean, median, standard deviation, and other key statistics
- **Wind Rose Diagrams**: Visualize wind direction frequency and intensity
- **Pattern Detection**: Identify calm periods, strong wind events, and daily patterns
- **Power Density Calculation**: Estimate wind energy potential
- **Time Series Visualization**: Plot wind speed and direction over time
- **Gust Analysis**: Calculate gust factors and identify peak wind events

## Installation

1. Clone the repository:
```bash
git clone https://github.com/jjherlan/jamaicca_bay_wind_data.git
cd jamaicca_bay_wind_data
```

2. Install dependencies:
```bash
pip install -r requirements.txt
```

## Quick Start

### Generate and Analyze Sample Data

```python
from wind_analyzer import WindDataAnalyzer
from visualizer import WindDataVisualizer
from generate_sample_data import generate_sample_data

# Generate sample data
data = generate_sample_data(num_samples=1000)

# Initialize analyzer
analyzer = WindDataAnalyzer(data)

# Get basic statistics
stats = analyzer.get_basic_statistics()
print(f"Mean wind speed: {stats['mean_speed']:.2f} m/s")

# Generate summary report
print(analyzer.get_summary_report())

# Create visualizations
visualizer = WindDataVisualizer()
visualizer.plot_time_series(data, save_path='wind_timeseries.png')
```

### Analyze Your Own Data

```python
from wind_analyzer import WindDataAnalyzer

# Load data from CSV
analyzer = WindDataAnalyzer()
analyzer.load_data('your_wind_data.csv')

# Or load from dictionary
data_dict = {
    'timestamp': [...],
    'wind_speed': [...],
    'wind_direction': [...]
}
analyzer.load_from_dict(data_dict)

# Run analysis
stats = analyzer.get_basic_statistics()
direction, percentage = analyzer.get_prevailing_direction()
```

## Data Format

The analyzer expects data in the following format:

| Column | Type | Description | Unit |
|--------|------|-------------|------|
| timestamp | datetime | Observation time | - |
| wind_speed | float | Wind speed | m/s |
| wind_direction | float | Wind direction (0° = North) | degrees |

## Example Analysis

Run the complete example analysis:

```bash
python example_analysis.py
```

This will:
1. Generate sample wind data (720 hours = 30 days)
2. Calculate comprehensive statistics
3. Detect wind events (calm periods and strong winds)
4. Analyze daily patterns
5. Generate 5 visualization plots

## Available Analyses

### Statistical Measures
- `get_basic_statistics()`: Mean, median, std dev, min, max
- `get_prevailing_direction()`: Most common wind direction
- `calculate_gust_factor()`: Ratio of peak to mean speeds

### Wind Events
- `detect_calm_periods(threshold)`: Identify low-wind periods
- `detect_strong_wind_events(threshold)`: Identify high-wind events

### Temporal Patterns
- `analyze_daily_pattern()`: Hourly wind statistics
- `get_wind_rose_data()`: Directional frequency distribution

### Energy Analysis
- `calculate_power_density()`: Wind energy potential (W/m²)

## Visualizations

The `WindDataVisualizer` class provides:
- **Time Series Plots**: Wind speed and direction over time
- **Wind Rose Diagrams**: Polar plot of directional frequencies
- **Distribution Histograms**: Wind speed probability distributions
- **Daily Patterns**: Hourly average wind speeds
- **Power Density Plots**: Wind energy potential over time

## Testing

Run the unit tests:

```bash
python -m unittest test_wind_analyzer.py
```

Or with verbose output:

```bash
python -m unittest test_wind_analyzer.py -v
```

## Project Structure

```
jamaicca_bay_wind_data/
├── README.md                    # This file
├── requirements.txt             # Python dependencies
├── wind_analyzer.py             # Core analysis module
├── visualizer.py                # Visualization utilities
├── generate_sample_data.py      # Sample data generation
├── example_analysis.py          # Complete usage example
└── test_wind_analyzer.py        # Unit tests
```

## Requirements

- Python 3.7+
- numpy >= 1.21.0
- pandas >= 1.3.0
- matplotlib >= 3.4.0
- scipy >= 1.7.0

## Contributing

Contributions are welcome! Please feel free to submit pull requests or open issues for bugs and feature requests.

## License

This project is open source and available under the MIT License.

## Author

Jamaica Bay Environmental Monitoring Team

## Acknowledgments

This tool was developed for analyzing wind data from environmental sensors deployed in Jamaica Bay to support environmental research and renewable energy assessments.
