package com.viglink.diffbotsequencemodel.features;

import cc.mallet.pipe.Pipe;
import cc.mallet.types.Instance;
import cc.mallet.types.Token;
import cc.mallet.types.TokenSequence;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;


public class TagName extends Pipe implements Serializable
{

    public TagName() {}

    // Too dangerous with both arguments having the same type
    //public RegexMatches (String regex, String feature) {
    //this (Pattern.compile (regex), feature);
    //}


    public Instance pipe (Instance carrier)
    {
        TokenSequence ts = (TokenSequence) carrier.getData();
        String firstTag = ts.get(0).getText();

        for (int i = 0; i < ts.size(); i++) {
            Token t = ts.get(i);
            t.setFeatureValue("tagName@"+firstTag,1.0);
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
