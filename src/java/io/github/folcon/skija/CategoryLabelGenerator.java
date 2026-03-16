package io.github.folcon.skija;

import org.jfree.chart.labels.CategoryItemLabelGenerator;
import org.jfree.data.category.CategoryDataset;

public class CategoryLabelGenerator implements CategoryItemLabelGenerator {
    public String generateRowLabel(CategoryDataset dataset, int row) {
        return "";
    }

    public String generateColumnLabel(CategoryDataset dataset, int column) {
        return "";
    }

    public String generateLabel(CategoryDataset dataset, int row, int column) {
        Number d = dataset.getValue(row, column);

        if (d.intValue() == 0) {
            return "";
        } else {
            return d.toString();
        }
    }

}
