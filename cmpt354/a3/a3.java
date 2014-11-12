/* author:   Colin Woodbury - 301238755
 * created:  2014 November 11 @ 16:01
 * modified: 2014 November 12 @ 09:09
 *
 * This is a terrible language and it shouldn't be taught.
 * Must be run with: `java -cp "mysql-connector-java-5.1.34/*:." a3`
 */

import java.io.File;
import java.sql.*;
import java.util.*;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import types.Customer;
import types.Customers;

// --- //

public class a3 {
    private static final String filename = "a3.xml";

    public static void main(String[] args) {
        Customers customers;

        loadDriver();
        customers = getCustomers();

        toXML(customers);
        //printCustomers(customers);

        System.out.println("Done.");
    }

    public static void loadDriver() {
        try {
            System.out.println("Loading the Driver...");
            Class.forName("com.mysql.jdbc.Driver").newInstance();
        } catch (Exception e) {
            System.out.println("Failed to load the Driver.");
        }        
    }

    public static Customers getCustomers() {
        Connection c;
        Statement s;
        ResultSet rs;
        List<Customer> customers = new LinkedList<Customer>();
        Customer cu;
        Customers cs = new Customers();

        String query = "select c.sin, c.last_name, c.first_name, sum(a.balance)" +
            " from customers c" +
            " inner join cust_accts ca on c.sin = ca.sin" +
            " inner join accounts a on ca.account_num = a.account_num" +
            " inner join branches b on a.branch_num = b.branch_num" +
            " group by c.sin;";

        try {
            c = DriverManager.getConnection("jdbc:mysql://localhost/354a2?" +
                                            "user=root&password=r00t");

            System.out.println("Connection successful.");

            s  = c.createStatement();
            rs = s.executeQuery(query);

            while(rs.next()) {
                cu = new Customer();
                cu.setSin(rs.getInt(1));
                cu.setLastName(rs.getString(2));
                cu.setFirstName(rs.getString(3));
                cu.setAssets(rs.getDouble(4));

                customers.add(cu);
            }
        } catch (SQLException e) {
            System.out.println("SQLException: " + e.getMessage());
            System.out.println("SQLState: " + e.getSQLState());
            System.out.println("VendorError: " + e.getErrorCode());
        }
       
        cs.setCustomers(customers);

        return cs;
    }

    public static void toXML(Customers customers) {
        try {
            JAXBContext context = JAXBContext.newInstance(Customers.class);
            Marshaller m = context.createMarshaller();
            //for pretty-print XML in JAXB
            m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);

            m.marshal(customers, System.out);

            // Write to File
            //m.marshal(emp, new File(FILE_NAME));
        } catch (JAXBException e) {
            e.printStackTrace();
        }
    }

    // For testing.
    public static void printCustomers(List<Customer> customers) {
        for(Customer c : customers) {
            System.out.printf("%d %s %s %.2f\n",
                              c.getSin(),
                              c.getLastName(),
                              c.getFirstName(),
                              c.getAssets());
        }
    }
}
