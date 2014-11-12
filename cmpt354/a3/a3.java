/* author:   Colin Woodbury - 301238755
 * created:  2014 November 11 @ 16:01
 * modified: 2014 November 11 @ 22:13
 *
 * This is a terrible language and it shouldn't be taught.
 * Must be run with: `java -cp "mysql-connector-java-5.1.34/*:." a3`
 */

import java.sql.*;

public class a3 {
    public static void main(String[] args) {
        Connection c = null;
        Statement s;
        ResultSet rs;

        String query = "select c.sin, c.last_name, c.first_name, sum(a.balance)" +
            " from customers c" +
            " inner join cust_accts ca on c.sin = ca.sin" +
            " inner join accounts a on ca.account_num = a.account_num" +
            " inner join branches b on a.branch_num = b.branch_num" +
            " group by c.sin;";

        // Results
        int sin;
        String lastName;
        String firstName;
        double assets;

        try {
            System.out.println("Loading the Driver...");
            Class.forName("com.mysql.jdbc.Driver").newInstance();
        } catch (Exception ex) {
            System.out.println("Failed to load the Driver.");
        }

        try {
            c = DriverManager.getConnection("jdbc:mysql://localhost/354a2?" +
                                            "user=root&password=r00t");

            System.out.println("Connection successful.");

            s  = c.createStatement();
            rs = s.executeQuery(query);

            while(rs.next()) {
                sin       = rs.getInt(1);
                lastName  = rs.getString(2);
                firstName = rs.getString(3);
                assets    = rs.getDouble(4);

                System.out.printf("%d %s %s %.2f\n",
                                  sin, lastName, firstName, assets);
            }
        } catch (SQLException ex) {
            // handle any errors
            System.out.println("SQLException: " + ex.getMessage());
            System.out.println("SQLState: " + ex.getSQLState());
            System.out.println("VendorError: " + ex.getErrorCode());
        }

        System.out.println("Done.");
    }
}
