import * as React from 'react';

interface IFullReceipt
{
    date: string;
    vendor: number;
    items: IFullReceiptItem[]
    amount: number;
}

interface IFullReceiptItem
{
    name: string;
    price: number;
}

interface IFullReceiptProps
{
    vendors: IVendorProps[]
}

interface IVendorProps
{
    vendorId: number;
    vendorName: string;
}

class FullReceipt extends React.Component<IFullReceiptProps, IFullReceipt> {
    constructor(props: IFullReceiptProps) {
        super(props);
        this.state = {
            date: "",
            vendor: 0,
            items: [
                { name: "Item1", price: 100 }
            ],
            amount: 0
        };
    }

    public render() {
        return (
            <div className="form">
                <div className="form-group">
                    <label>Date</label>
                    <input type="text" className="form-control" placeholder="01/01/2000 12:00 AM" value={this.state.date} />
                </div>
                <div className="form-group">
                    <label>Vendor</label>
                    <select className="form-control">
                        {this.props.vendors.map((vendor) => 
                            <option value={vendor.vendorId} key={vendor.vendorId} selected={this.state.vendor == vendor.vendorId}>{vendor.vendorName}</option>
                        )}
                    </select>
                </div>
                <div className="form-group">
                    <label>Items</label>
                    {this.state.items.map((item) =>
                        <div className="form-group">
                            <input type="text" className="form-control" placeholder="Apples" value={item.name} />
                        </div>
                    )}
                </div>
                <div className="form-group">
                    <label>Amount</label>
                    <div className="input-group">
                        <div className="input-group-prepend">
                            <span className="input-group-text">$</span>
                        </div>
                        <input type="text" className="form-control" placeholder="100.00" value={this.state.amount} />
                    </div>
                </div>
            </div>
        );
    }    
}

export default FullReceipt;