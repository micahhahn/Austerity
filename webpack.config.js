const path = require("path");
const webpack = require("webpack");

module.exports = {
    entry: {
        bundle: "./web/ts/index.tsx"
    },

    output: {
        filename: "[name].js",
        path: path.resolve(__dirname, "dist/static/scripts/")
    },

    // Enable sourcemaps for debugging webpack's output.
    devtool: "source-map",

    resolve: {
        // Add '.ts' and '.tsx' as resolvable extensions.
        extensions: [".ts", ".tsx", ".js", ".json"]
    },

    module: {
        rules: [
            // All files with a '.ts' or '.tsx' extension will be handled by 'awesome-typescript-loader'.
            { 
                test: /\.ts|\.tsx?$/, 
                loader: "awesome-typescript-loader",
                include: [
                    path.resolve(__dirname, "web/")
                ]
            },

            // All output '.js' files will have any sourcemaps re-processed by 'source-map-loader'.
            { 
                enforce: "pre", 
                test: /\.js$/, 
                loader: "source-map-loader",
                include: [
                    path.resolve(__dirname, "web/")
                ]
            }
        ]
    }
};