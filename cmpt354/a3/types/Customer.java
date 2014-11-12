package types;

import javax.xml.bind.annotation.*;

// --- //

@XmlRootElement(name = "Customer")
@XmlType(propOrder = {"sin", "lastName", "firstName", "assets"})
public class Customer {
    private int sin;
    private String lastName;
    private String firstName;
    private double assets;

    public int getSin() { return sin; }
    public void setSin(int sin) { this.sin = sin; }

    public String getLastName() { return lastName; }
    public void setLastName(String lastName) { this.lastName = lastName; }

    public String getFirstName() { return firstName; }
    public void setFirstName(String firstName) { this.firstName = firstName; }

    public double getAssets() { return assets; }
    public void setAssets(double assets) { this.assets = assets; }
}
