/**
 * @author: Dr. rer. nat. George Assaf (Brandenburg University of Technology (BTU), Cottbus, Germany)
 * @version: 1.0
 * @date: 2025-10-26
 * @description: Main application entry point for the MASSP project.
 */


package com.example.massp;

 
public class App 
{
    public static void main( String[] args )
    {
       // System.out.println( "Hello World!" );
       MASSP massp = new MASSP();

       massp.solve();
    }
}
