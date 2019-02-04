import * as React from 'react';
import { render } from 'react-dom';
import App from './components/App';
import {FullReceipt, IFullReceiptProps} from './components/FullReceipt';

declare global {
    interface Window { Austerity: any; }
}

window.Austerity = window.Austerity || (() => {
    return {
        renderFullReceipt: (root: HTMLElement, props: IFullReceiptProps) => {
            render(React.createElement(FullReceipt, props), root)
        }
    };
})();