import * as React from 'react';
import * as Ajax from '../../../build/Endpoints';

interface AppProps {
    receipts: Array<Ajax.FullReceipt_Identity>,
};

export default function(props: AppProps) {
    return  <div>
                <h1>Hello World</h1>
                <table className="receipts">
                    <tbody>
                        {props.receipts.map(r => 
                            <tr key={r.receiptId}>
                                <td>{r.date}!!</td>
                                <td>{r.amount}</td>
                                <td>{r.vendor}</td>
                            </tr> 
                        )}
                    </tbody>
                </table>
            </div>;
};