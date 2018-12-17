const path = require('path');
const ExtractTextPlugin = require('extract-text-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
  entry: ['./src/index.js'],
  mode: process.env.NODE_ENV,
  output: {
    publicPath: '/'
  },
  resolve: {
    modules: [
      path.join(__dirname, 'src'),
      'node_modules'
    ],
    extensions: ['.js', '.elm']
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        use: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: [
            { loader: 'css-loader', options: { importLoaders: 1 } },
            'postcss-loader',
          ],
        }),
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
          options: { debug: process.env.NODE_ENV === 'development' }
        }
      }
    ],
  },
  plugins: [
    new ExtractTextPlugin('styles.css', {
      disable: process.env.NODE_ENV === 'development',
    }),
    new HtmlWebpackPlugin({
      filename: 'index.html',
      template: 'src/index.html',
    }),
  ],

  devServer: {
    contentBase: path.join(__dirname, 'dist'),
    compress: true,
    port: 8000,
    historyApiFallback: true
  }
}
