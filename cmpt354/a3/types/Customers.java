package types;

import java.util.*;
import javax.xml.bind.annotation.*;
import types.Customer;

// --- //

@XmlRootElement(name = "CustSummary")
@XmlSeeAlso({Customer.class})
public class Customers {
    private List<Customer> customers;

    @XmlElementWrapper(name="Customers")
    @XmlElementRef()
    public List<Customer> getCustomers() {
        return customers;
    }

    public void setCustomers(List<Customer> cs) {
        this.customers = cs;
    }
}
