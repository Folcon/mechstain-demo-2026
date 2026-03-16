package io.github.folcon.skija;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.CategoryAxis;
import org.jfree.chart.axis.CategoryLabelPositions;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.block.BlockBorder;
import org.jfree.chart.labels.CategoryItemLabelGenerator;
import org.jfree.chart.labels.ItemLabelAnchor;
import org.jfree.chart.labels.ItemLabelPosition;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.category.BarRenderer;
import org.jfree.chart.title.DateTitle;
import org.jfree.chart.title.TextTitle;
import org.jfree.chart.ui.ApplicationFrame;
import org.jfree.chart.ui.TextAnchor;
import org.jfree.data.category.CategoryDataset;
import org.jfree.data.category.DefaultCategoryDataset;

import javax.swing.*;
import java.awt.*;


public class Chart extends JPanel {
    public JFreeChart chart;
    public ChartPanel chart_panel;
    public Chart() {
        DefaultCategoryDataset dataset = new DefaultCategoryDataset();
        dataset.addValue(7445, "JFreeSVG", "Warm-up");
        dataset.addValue(24448, "Batik", "Warm-up");
        dataset.addValue(4297, "JFreeSVG", "Test");
        dataset.addValue(21022, "Batik", "Test");

        chart = createChart((CategoryDataset) dataset);
        chart_panel = new ChartPanel((JFreeChart) null);
        chart_panel.setChart(chart);
        chart_panel.setBounds(39, 193, 419, 309);

//        ChartPanel chart_panel = new ChartPanel(chart, false);
//        chart_panel.setFillZoomRectangle(true);
//        chart_panel.setMouseWheelEnabled(true);
//        chart_panel.setPreferredSize(new Dimension(500, 270));
    }

    public static JFreeChart createChart(CategoryDataset dataset) {
        JFreeChart chart = ChartFactory.createBarChart(
                "Performance: JFreeSVG vs Batik", null /* x-axis label*/,
                "Milliseconds" /* y-axis label */, dataset);
        chart.addSubtitle(new TextTitle("Time to generate 1000 charts in SVG "
                + "format (lower bars = better performance)"));
        chart.setBackgroundPaint(Color.WHITE);
        CategoryPlot plot = (CategoryPlot) chart.getPlot();

        NumberAxis rangeAxis = (NumberAxis) plot.getRangeAxis();
        rangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
        BarRenderer renderer = (BarRenderer) plot.getRenderer();
        renderer.setDrawBarOutline(false);
        chart.getLegend().setFrame(BlockBorder.NONE);
        return chart;
    }

    public static JPanel panel() {
        JPanel panel = new JPanel();
        panel.setBackground(new Color(255, 102, 51));
        panel.setBounds(50, 64, 955, 888);
        panel.setLayout(null);

        Chart chart = new Chart();

        panel.add(chart.chart_panel, BorderLayout.CENTER);

        panel.validate();

//
//        Chart c = new Chart();
//
//        c.setVisible(true);
//
//        c.setBounds(10, 10, 50, 50);
//        c.setSize(100, 100);
//        c.setLayout(null);
//        c.setVisible(true);
//        return c;
        return panel;
    }

    public static JPanel foo() {
        DefaultCategoryDataset dataset = new DefaultCategoryDataset();
        dataset.addValue(7445, "JFreeSVG", "Warm-up");
        dataset.addValue(24448, "Batik", "Warm-up");
        dataset.addValue(4297, "JFreeSVG", "Test");
        dataset.addValue(21022, "Batik", "Test");


        //Plot type: Stacked Bar Graph.
        JFreeChart chart  = ChartFactory.createStackedBarChart("Title","", "", dataset, PlotOrientation.VERTICAL,true, true,false);

        DateTitle dateTitle = new DateTitle(); //Set current date as the subtitle of the chart
        chart.addSubtitle(dateTitle);

        CategoryPlot plot = chart.getCategoryPlot();
        plot.getRenderer().setSeriesPaint(0,new Color(0,255,0)); //4 series to show. (Approved, Negotiaion, Cancelled, Completed)
        plot.getRenderer().setSeriesPaint(1,new Color(0,0,255));
        plot.getRenderer().setSeriesPaint(2,new Color(255,0,0));
        plot.getRenderer().setSeriesPaint(3, new Color(255,255,0));

        CategoryAxis xAxis = plot.getDomainAxis();
        xAxis.setCategoryLabelPositions(CategoryLabelPositions.UP_45); //X-Axis Labels will be inclined at 45degree
        xAxis.setLabel("X Axis Label");

        ValueAxis rangeAxis = plot.getRangeAxis();
        rangeAxis.setAutoRange(true); // Y-Axis range will be set automatically based on the supplied data
        rangeAxis.setLabel("Y Axis Label");

        BarRenderer renderer = (BarRenderer)plot.getRenderer();
        renderer.setMaximumBarWidth(.1); //making sure that if there is only one bar, it does not occupy the entire available width

        renderer.setDefaultItemLabelGenerator(new CategoryLabelGenerator());
        renderer.setDefaultItemLabelsVisible(true);

        ItemLabelPosition p = new ItemLabelPosition(ItemLabelAnchor.CENTER, TextAnchor.BOTTOM_CENTER);
        renderer.setPositiveItemLabelPositionFallback(p);

        ChartPanel chPanel = new ChartPanel(chart); //creating the chart panel, which extends JPanel
        chPanel.setPreferredSize(new Dimension(785, 440)); //size according to my window
        chPanel.setMouseWheelEnabled(true);
        JPanel jPanel = new JPanel();
        jPanel.setBackground(Color.CYAN);
        jPanel.add(chPanel);
        jPanel.setVisible(true);

        JLabel label = new JLabel("TEST");
        jPanel.add(label);

        return jPanel;
    }

    public static javax.swing.JPanel bar() {
        var panel = new javax.swing.JPanel();
        panel.setLayout(new javax.swing.BoxLayout(panel, BoxLayout.PAGE_AXIS));
        var y = -25;

        var label = new JLabel("iter 0");
        label.setBounds(10, y += 35, 160, 25);
        panel.add(label);

        var combobox = new javax.swing.JComboBox<>(new String[]{"javax.swing.JComboBox"});
        combobox.setBounds(10, y += 35, 160, 25);
        panel.add(combobox);

//        var checkbox = new javax.swing.JCheckBox("Checkbox", false);
//        checkbox.setBounds(10, y += 35, 160, 25);
//        panel.add(checkbox);
//
//        checkbox = new javax.swing.JCheckBox("Checkbox", true);
//        checkbox.setBounds(10, y += 35, 160, 25);
//        panel.add(checkbox);
//
//        var radio = new javax.swing.JRadioButton("JRadioButton", false);
//        radio.setBounds(10, y += 35, 160, 25);
//        panel.add(radio);
//
//        radio = new javax.swing.JRadioButton("JRadioButton", true);
//        radio.setBounds(10, y += 35, 160, 25);
//        panel.add(radio);
//
        var slider = new javax.swing.JSlider() {
            @Override public java.awt.Point getMousePosition() throws HeadlessException { return null; }
        };
        slider.setBounds(10, y += 35, 160, 25);
        panel.add(slider);

        var textfield = new javax.swing.JTextField("JTextField");
        textfield.setBounds(10, y += 35, 160, 25);
        panel.add(textfield);

        var textarea = new javax.swing.JTextArea("JTextArea");
        textarea.setBounds(10, y += 35, 160, 25);
        panel.add(textarea);

        var progress = new javax.swing.JProgressBar();
        progress.setValue(30);
        progress.setBounds(10, y += 35, 160, 25);
        panel.add(progress);

        panel.setSize(180, y + 35);
        return panel;
    }
}

