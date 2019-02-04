import * as React from 'react';

interface IFullReceipt
{
    date: string;
    vendor: string;
    items: IFullReceiptItem[]
    amount: string;
}

interface IFullReceiptItem
{
    name: string;
    price: string;
}

export interface IFullReceiptProps
{
    vendors: IVendorProps[]
}

interface IVendorProps
{
    vendorId: number;
    vendorName: string;
}

export class FullReceipt extends React.Component<IFullReceiptProps, IFullReceipt> {
    constructor(props: IFullReceiptProps) {
        super(props);
        this.state = {
            date: "",
            vendor: "",
            items: [
                { name: "", price: "" }
            ],
            amount: ""
        };
    }

    newItem = (e: React.MouseEvent<HTMLElement>) => {
        var items = [...this.state.items];
        items.push({ name: "", price: "" });   
        this.setState({items: items});
    }

    itemChange = <T extends keyof IFullReceiptItem>(s: T, i: number, e: React.ChangeEvent<HTMLInputElement>) => {
        let items = [...this.state.items];
        items[i][s] = e.target.value;
        this.setState({items: items})
    }

    public render() {
        return (
            <div className="form">
                <div className="form-group">
                    <label>Date</label>
                    <input type="text" name="date" className="form-control" placeholder="01/01/2000 12:00 AM" value={this.state.date} onChange={e => this.setState({date: e.target.value})} />
                </div>
                <div className="form-group">
                    <label>Vendor</label>
                    <select name="vendor" className="form-control" onChange={e => this.setState({ vendor: e.target.value})}>
                        {this.props.vendors.map((vendor) => 
                            <option value={vendor.vendorId} key={vendor.vendorId}>{vendor.vendorName}</option>
                        )}
                    </select>
                </div>
                <div className="form-group">
                    <label>Items</label>
                    <div className="row">
                        <div className="col">Name</div>
                        <div className="col">Price</div>
                    </div>
                    {this.state.items.map((item, index) =>
                        <div className="row" key={"items[" + index + "]"}>
                            <div className="col">
                                <input type="text" className="form-control" placeholder="Apples" value={item.name} onChange={e => this.itemChange("name", index, e)} />
                            </div>
                            <div className="col">
                                <div className="input-group">
                                    <div className="input-group-prepend">
                                        <span className="input-group-text">$</span>
                                    </div>
                                    <input type="text" className="form-control" placeholder="100" value={item.price} onChange={e => this.itemChange("price", index, e)} />
                                </div>
                            </div>
                        </div>
                    )}
                    <button onClick={this.newItem}>New Item</button>
                </div>
                <div className="form-group">
                    <label>Amount</label>
                    <div className="input-group">
                        <div className="input-group-prepend">
                            <span className="input-group-text">$</span>
                        </div>
                        <input name="amount" type="text" className="form-control" placeholder="100.00" value={this.state.amount} onChange={e => this.setState({amount: e.target.value})} />
                    </div>
                </div>
                <button type="button" className="btn btn-primary">Create Receipt</button>
            </div>
        );
    }    
}

export default FullReceipt;