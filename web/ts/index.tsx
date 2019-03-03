import * as React from 'react';

import { render } from 'react-dom';
import App from './components/App';
import * as Ajax from '../../build/Endpoints';
import {FullReceipt, IFullReceiptProps} from './components/FullReceipt';

declare global {
    interface Window { Austerity: any; }
}

window.Austerity = window.Austerity || (() => {
    return {
        bootstrapApp: (root: HTMLElement) => {
            Ajax.GetReceipts(rs => {
                render(React.createElement(App, { receipts: rs }), root);
            }, () => {});
            
        }
    };
})();