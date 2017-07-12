package com.viglink.diffbotsequencemodel.features;

import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.io.*;

import cc.mallet.pipe.*;
import cc.mallet.types.*;


public class TokenTextLength extends Pipe implements Serializable
{


    public TokenTextLength() {}

    // Too dangerous with both arguments having the same type
    //public RegexMatches (String regex, String feature) {
    //this (Pattern.compile (regex), feature);
    //}


    public Instance pipe (Instance carrier)
    {
        TokenSequence ts = (TokenSequence) carrier.getData();

        for (int i = 0; i < ts.size(); i++) {
            Token t = ts.get(i);
            Integer sLength = t.getText().length();

            if(sLength > 0 && sLength <= 5){
                t.setFeatureValue("1to5chars",1.0);
            }
            else if(sLength > 5 && sLength <=10){
                t.setFeatureValue("6to10chars",1.0);
            }
            else if(sLength >10 && sLength <=15){
                t.setFeatureValue("11to15chars",1.0);
            }
            else if(sLength > 15 && sLength <=20){
                t.setFeatureValue("16to20chars",1.0);
            }
            else {
                t.setFeatureValue("over20chars",1.0);
            }

        }
        return carrier;
    }


    // Serialization

    private static final long serialVersionUID = 1;
    private static final int CURRENT_SERIAL_VERSION = 0;

    private void writeObject (ObjectOutputStream out) throws IOException {
        out.writeInt(CURRENT_SERIAL_VERSION);
        //out.writeObject(regex);
        //out.writeObject(feature);
    }

    private void readObject (ObjectInputStream in) throws IOException, ClassNotFoundException {
        int version = in.readInt ();
        //regex = (Pattern) in.readObject();
        //feature = (String) in.readObject();
    }


}
