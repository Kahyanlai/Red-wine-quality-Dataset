
# 🍷 Wine Quality Prediction using Aggregation Functions (R)

This project aims to predict the quality of red wine using statistical data preprocessing and advanced aggregation models in R. The prediction is based on multiple physicochemical attributes and includes preprocessing steps, outlier removal, skewness correction, normalization, and model fitting using several aggregation strategies.

📺 **Presentation Video:** [YouTube](https://youtu.be/JAAYWde9SGw)

---

## 📁 Dataset

- **Source:** `RedWine.txt` file containing 1,599 observations of red wine samples.
- **Variables:** Citric Acid, Chlorides, Total Sulfur Dioxide, pH, Alcohol, Quality

---

## ⚙️ Project Structure

1. **Data Understanding & Exploration**
   - Scatter plots, histograms, and boxplots to visualize distribution and identify outliers.
2. **Data Cleaning**
   - Removed ~15.8% outliers using IQR method.
3. **Data Transformation**
   - Applied power, sqrt, reciprocal transformations to reduce skewness.
   - Normalization via Min-Max scaling.
4. **Model Building**
   - Aggregation functions used:
     - Weighted Arithmetic Mean (QAM)
     - Power Means (p = 0.5 and 2)
     - Ordered Weighted Average (OWA)
   - Models evaluated based on error and correlation.
   - **Best Model Chosen:** Quadratic Mean (QM) with highest accuracy and lowest error.
5. **Prediction**
   - A new wine sample is transformed and predicted using the chosen QM model.

---

## 📈 Final Prediction Result

The model predicted the wine quality score for a new sample (features: Citric Acid = 0.9, Chlorides = 0.65, pH = 2.53, Alcohol = 7.1) as:

```R
Predicted wine quality: 6
