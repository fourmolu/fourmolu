const fs = require('fs/promises')
const path = require('path')

const baseConfig = {
  entry: './index.js',
  output: {
    filename: 'worker.js',
  },
}

module.exports = (env, argv) => {
  const config = {
    ...baseConfig,
    mode: argv.mode || 'development',
  }

  return config
}
