import * as React from 'react';
import { render } from 'react-dom';
import App from './components/App';
import FullReceipt from './components/FullReceipt';

render(
    <FullReceipt vendors={[{ vendorId: 1, vendorName: "First" }, {vendorId: 2, vendorName: "Second"}]} />,
    document.getElementById('example'),
);