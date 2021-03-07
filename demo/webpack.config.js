var path = require('path');

module.exports = {

  entry: {
    require_material_components_elm_js_imports: './src/require_material_components_elm_js_imports.js',
    index: './src/index.js'
  },

  output: {
    filename: '[name].bundle.js',
    path: path.resolve(__dirname, 'dist')
  },

  module: {
    rules: [{
        test: /\.html$/,
        exclude: /node_modules/,
        use: [{
          loader: 'file-loader',
          options: {
            name: '[name].[ext]',
            debug: true
          }
        }]
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [{
          loader: 'elm-webpack-loader',
          options: {}
        }]
      },
      {
        test: /\.css$/i,
        use: ['style-loader', 'css-loader'],
      },
      {
        test: /\.(png|jpe?g|gif|svg)$/,
        use: [{
            loader: 'file-loader',
            options: {
              name: '[path][name].[ext]',
              context: 'src',
              debug: true
            }
          }
        ]
      }
    ]
  },

  devServer: {
    inline: true,
    stats: 'errors-only'
  }
};
